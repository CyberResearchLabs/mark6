/*
 * Created by David Lapsley on Mon Jun 6 2011.
 *
 * Copyright 2011 MIT Haystack Observatory 
 *  
 * This file is part of mark6.
 *
 * mark6 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the Li!cense, or
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

#ifndef _PFILEWRITER_H_
#define _PFILEWRITER_H_

// C includes.
#include <poll.h>

// C++ includes
#include <list>

// Framework includes.

// Local includes
#include <mark6.h>
#include <file_writer.h>

class StatsWriter;

//! Manages the high speed writing of data to file.
//! Includes a circular buffer for storing buffers to be written, as well as
//! a state machine that controls the operation of the thread. The class
//! is an "active" object that runs in its own thread of execution. External
//! objects can interact with it via the start(), join(), cmd_XXX(), 
//! write(), open(), and close() methods.
class PFileWriter: public FileWriter {

 public:
  //---------------------------------------------------------------------------
  // Public API
  //---------------------------------------------------------------------------

  //! Constructor
  //! \param id A unique id for this object. Used for logging.
  //! \param write_block_size The size of the indidivual write blocks to be
  //!        written to disk. WRITE_BLOCK bytes of data will be written to
  //!        disk each time write_block() is called.
  //! \param write_blocks The total number of write blocks to buffer
  //!        internally.
  //! \param capture_files A list of capture files to use for the stored data.
  //! \param poll_timeout \todo Obsolete parameter.
  //! \param sw A pointer to a StatsWriter object. Performance data will be 
  //!        logged using this object.
  //! \param command_interval The main executin thread in run() will attempt
  //!        to check for new commands every command_interval seconds. The
  //!        actual interval between checks may be larger than this if the 
  //!        execution thread spends longer than command_interval processing
  //!        individual tasks.
  PFileWriter(const int id,
	     const int write_block_size,
	     const int write_blocks,
	     const std::list<std::string>& capture_files,
	     const int poll_timeout,
	     StatsWriter * const sw,
	     const double command_interval);

  //! Destructor.
  virtual ~PFileWriter();

  using FileWriter::start;
  using FileWriter::join;
  using FileWriter::cmd_stop;
  using FileWriter::cmd_write_to_disk;

  //! Custom API. Open capture file on disk.
  virtual int open();

  //! Custom API. Close capture file on disk.
  virtual int close();

  using FileWriter::write;
  using FileWriter::write_unbuffered;

  using FileWriter::malloc_buffer;
  using FileWriter::free_buffer;

 protected:
  //---------------------------------------------------------------------------
  // Internal data members
  //---------------------------------------------------------------------------


  //---------------------------------------------------------------------------
  // Internal methods
  //---------------------------------------------------------------------------

  //! Writes  a block to disk.
  //! \param buf The buffer to write to disk.
  //! \param buf_len The length of the buffer.
  //! return true if successful, false if not.
  virtual bool write(boost::uint8_t* buf, const int buf_len);
};

#endif // _PFILEWRITER_H_
