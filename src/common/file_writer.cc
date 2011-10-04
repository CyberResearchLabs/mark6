/*
 * Created by David Lapsley on Mon Jun 6 2011.
 *
 * Copyright 2011 MIT Haystack Observatory 
 *  
 * This file is part of mark6.
 *
 * mark6 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * mark6 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with mark6.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

// C includes
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

// C++ includes.
#include <sstream>
#include <iostream>

// Framework includes.
#include <boost/crc.hpp>
#include <boost/filesystem.hpp>

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <file_writer.h>
#include <stats_writer.h>

using namespace boost::filesystem;

FileWriter::FileWriter(const int id,
		       const int write_block_size,
		       const int  write_blocks,
		       const std::string& capture_file,
		       const int poll_timeout,
		       StatsWriter* const sw,
		       const double command_interval):
  Threaded(id, command_interval),
  _WRITE_BLOCK_SIZE(write_block_size),
  _WRITE_BLOCKS(write_blocks),
  _POLL_TIMEOUT(poll_timeout),
  _sw(sw),
  _pfd(),
  _write_bufs(),
  _free_bufs(),
  _state (IDLE),
  _write_bufs_mutex(),
  _free_bufs_mutex(),
  _capture_file(capture_file)
{
  void* buf;
  for (int i=0; i<write_blocks; i++) {
    if (posix_memalign(&buf, getpagesize(), write_block_size) != 0) {
      LOG4CXX_ERROR(logger, "FileWriter buffer allocation failed.");
      throw std::string("Memalign failed.");
    }
    _free_bufs.push_back(static_cast<boost::uint8_t*>(buf));
  }
}

FileWriter::~FileWriter() {
  close();
}

void FileWriter::start() {
  _running = true;
  _thread = boost::thread(&FileWriter::run, this);
}

void FileWriter::join() {
  _thread.join();
}

void FileWriter::run() {
  LOG4CXX_INFO(logger, "FileWriter Running...");

  Timer run_timer;
  Timer command_timer;
  
  try {
    // Main processing loop.
    while (_running) {

      // State machine.
      switch (_state) {
      case WRITE_TO_DISK:
	if (command_timer.elapsed() > _command_interval) {
	  command_timer.restart();
	  continue;
	}
	handle_write_to_disk();
	break;

      case IDLE:
	if (command_timer.elapsed() > _command_interval) {
	  command_timer.restart();
	  continue;
	}
	handle_idle();
	break;

      case STOP:
	handle_stop();
	break;

      default:
	LOG4CXX_ERROR(logger, "Unknown state.");
	break;
      }
    }
    LOG4CXX_DEBUG(logger, "elapsed run time: " << run_timer.elapsed());
  } catch(std::exception &ex) {
    LOG4CXX_ERROR(logger, "error: " << ex.what());
  }
}

void FileWriter::cmd_stop() {
  LOG4CXX_INFO(logger, "Received STOP");
  _state = STOP;
}

void FileWriter::cmd_write_to_disk() {
  LOG4CXX_INFO(logger, "Received WRITE_TO_DISK");
  _state = WRITE_TO_DISK;
}

void FileWriter::handle_stop() {
  _running = false;
}

void FileWriter::handle_idle() {
  usleep(_command_interval*1000000);
}

void FileWriter::handle_write_to_disk() {
  write_block();
}

int FileWriter::open() {
  LOG4CXX_INFO(logger, "Opening FileWriter file: " << _capture_file);

  // Open files for each path.
  int ret=0;

#define FALLOCATE
#ifdef FALLOCATE
  _pfd.fd = ::open(_capture_file.c_str(), O_WRONLY | O_DIRECT, S_IRWXU);
#else
#ifdef DIRECT_BUFFER
  _pfd.fd = ::open(_capture_file.c_str(), O_WRONLY | O_CREAT | O_DIRECT,
		   S_IRWXU);
#else
  _pfd.fd = ::open(_capture_file.c_str(), O_WRONLY | O_CREAT, S_IRWXU);
#endif // DIRECT_BUFFER
#endif // FALLOCATE
  if (_pfd.fd<0) {
    LOG4CXX_ERROR(logger, "Unable to open file: " << _capture_file
		  << " - " << strerror(errno));
    _pfd.fd = -1;
    ret = -1;
  } else {
    LOG4CXX_DEBUG(logger, "File: " << _capture_file << " fd: "
		  << _pfd.fd);
    _pfd.events = POLLOUT;
  }

#ifdef FALLOCATE
  if (::lseek(_pfd.fd, 0, SEEK_SET) < 0) {
    LOG4CXX_ERROR(logger, "Unable to seek to beginning of file: " << _capture_file
		  << " - " << strerror(errno));
    _pfd.fd = -1;
    ret = -1;
  } else {
    LOG4CXX_DEBUG(logger, "Successfully seeked.");
  }
#endif // FALLOCATE

  // Debug message.
  LOG4CXX_DEBUG(logger, "pfd: " << _pfd.fd);

  return ret;
}

int FileWriter::close() {
  if ( (_pfd.fd>0) && (::close(_pfd.fd)<0) ) {
    LOG4CXX_ERROR(logger, "Unable to close fd: " << _pfd.fd
		  << " - " << strerror(errno));
    return -1;
  }
  return 0;
}

boost::uint8_t* FileWriter::malloc_buffer() {
  boost::system_time timeout = boost::get_system_time() 
    + boost::posix_time::milliseconds(_POLL_TIMEOUT*1000);

  boost::mutex::scoped_lock lock(_free_bufs_mutex); 
  while (_free_bufs.empty())
    if (!_read_cond.timed_wait(lock, timeout))
      return 0;
 
  boost::uint8_t* b = _free_bufs.front();
  _free_bufs.pop_front();

  return b;
}

bool FileWriter::free_buffer(boost::uint8_t* buf) {

  boost::system_time timeout = boost::get_system_time() 
    + boost::posix_time::milliseconds(_POLL_TIMEOUT*1000);

  // Never block on free because total buffer pool is fixed.
  // This assumes only buffers allocated from this FileWriter
  // are returned to it.
  boost::mutex::scoped_lock lock(_free_bufs_mutex);    
  _free_bufs.push_back(buf);
  _read_cond.notify_one();
  return true;
}

bool FileWriter::write(boost::uint8_t* b) {
  boost::mutex::scoped_lock lock(_write_bufs_mutex);
  _write_bufs.push_back(b);
// #define BLOCKING
#ifdef BLOCKING
  _write_cond.notify_one();
#endif
  return true;
}

void FileWriter::write_block() {
  boost::uint8_t* buf;
  boost::uint64_t buf_len;

#ifdef BLOCKING
  boost::system_time timeout = boost::get_system_time() 
  	+ boost::posix_time::milliseconds(_POLL_TIMEOUT*1000);
#endif

  {
    boost::mutex::scoped_lock lock(_write_bufs_mutex); 
#ifdef BLOCKING
    while (_write_bufs.empty())
    	if (!_write_cond.timed_wait(lock, timeout))
    		return;
#else
    if (_write_bufs.empty())
      return;
#endif
    buf = _write_bufs.front();
    // FIXME
    buf_len = _write_bufs.size();
    _write_bufs.pop_front();
  }

  write(buf, _WRITE_BLOCK_SIZE);
}

bool FileWriter::write(boost::uint8_t* buf,
		       const int buf_len) {
  // Write buffer to disk.
  int bytes_left = buf_len;
  int bytes_written = 0;
  while (bytes_left) {
    int nb = ::write(_pfd.fd, &buf[bytes_written], bytes_left);
    if (nb > 0) {
      bytes_left -= nb;
      bytes_written += nb;
    } else {
      LOG4CXX_ERROR(logger, "Write error: " << strerror(errno));
    }
  }
  if (_sw)
    _sw->update(1, bytes_written, 0, buf_len);
  free_buffer(buf);
}

bool FileWriter::write_unbuffered(boost::uint8_t* buf,
				  const boost::uint32_t len) {
  boost::uint64_t buf_len = 0;
  {
    boost::mutex::scoped_lock lock(_write_bufs_mutex);
    buf_len = _write_bufs.size();
  }
  // Write buffer to disk.
  int bytes_left = len;
  int bytes_written = 0;
  while (bytes_left) {
    int nb = ::write(_pfd.fd, &buf[bytes_written], bytes_left);
    if (nb > 0) {
      bytes_left -= nb;
      bytes_written += nb;
    } else {
      LOG4CXX_ERROR(logger, "Write error: " << strerror(errno));
    }
  }
  if (_sw)
    _sw->update(1, bytes_written, 0, buf_len);
  return true;
}

