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
#include <string.h>

// C++ includes.
#include <sstream>
#include <iostream>

// Framework includes.
#include <boost/crc.hpp>      // for boost::crc_basic, boost::crc_optimal
#include <cstddef>    // for std::size_t
#include <boost/filesystem.hpp>

using namespace boost::filesystem;

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <file_manager.h>


FileManager::FileManager(const std::string mid,
			 const std::string mount_point,
			 const std::string mount_prefix,
			 const int num_mount_points):
  MAX_QUEUE_SIZE(100),
  MESSAGE_SIZE(sizeof(ControlMessage)),
  _mq(create_only, mid.c_str(), MAX_QUEUE_SIZE, MESSAGE_SIZE), 
  _mid(mid),
  _mount_point(mount_point),
  _mount_prefix(mount_prefix),
  _num_mount_points(num_mount_points)
{
  _fds = new int[num_mount_points];
}

FileManager::~FileManager()
{
  close();
  delete [] _fds;
  message_queue::remove(_mid.c_str());
}

void FileManager::start()
{
  _thread = boost::thread(&FileManager::run, this);
}

void FileManager::run()
{
  // Main entry point.
  LOG4CXX_INFO(logger, "Running...");
  try {
    while (true) {
      ControlMessage m;
      unsigned int priority;
      std::size_t recvd_size;
      _mq.receive(&m, sizeof(m), recvd_size, priority);
      LOG4CXX_DEBUG(logger, "Received message type: " << m._type);

      if (m._type == STOP) {
	LOG4CXX_DEBUG(logger, "Received stop");
	break;
      }

      sleep(1);
    }
  } catch(interprocess_exception &ex) {
    LOG4CXX_ERROR(logger, "mq error: " << ex.what());
  }
}

void FileManager::join()
{
  _thread.join();
}

int FileManager::open(const std::string file_name)
{
  std::vector<std::string> paths;
  for (int i=0; i<_num_mount_points; ++i) {
    std::ostringstream ss;
    ss << _mount_point << "/" << _mount_prefix << i << "/" << file_name;
    paths.push_back(ss.str());
    ss.clear();
  }

  int ret=0;
  int i=0;
  BOOST_FOREACH(std::string p, paths) {
    LOG4CXX_DEBUG(logger, "path: " << p);

    int fd = ::open(p.c_str(),  O_WRONLY | O_CREAT | O_NONBLOCK, S_IRWXU);
    if (fd<0) {
      LOG4CXX_ERROR(logger, "Unable to open file: " << p << " - " 
		    << strerror(errno));
      _fds[i++] = -1;
      ret = -1;
    } else {
      _fds[i++] = fd;
    }
  }

  return ret;
}

int FileManager::close()
{
  int ret = 0, RET = 0;
  for (int i=0; i<_num_mount_points; ++i) {
    if (_fds[i] > 0) {
      if ( (ret=::close(_fds[i])) < 0) {
	LOG4CXX_ERROR(logger, "Unable to close fd: " << _fds[i] << " - " 
		      << strerror(errno));
	RET = ret;
      }
    }
  }
  return RET;
}

int FileManager::write(char* buf, int n)
{
  return 0;
}

int FileManager::read(char* buf, int n)
{
  return 0;
}


