/*
 *  File.h
 *
 *  Created by David Lapsley on Fri Feb 20 2004.
 *
 */

/* 
 * This software is released under the terms of the MIT License(included below). *  
 * Copyright (c) 2003 MIT Haystack Observatory 
 *  
 * Permission is hereby granted, free of charge, to any person obtaining a   
 * copy of this software and associated documentation files (the "Software"),    * to deal in the Software without restriction, including without limitation   
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   
 * and/or sell copies of the Software, and to permit persons to whom the  
 * Software is furnished to do so, subject to the following conditions: 
 *  
 * The above copyright notice and this permission notice shall be included in    * all copies or substantial portions of the Software. 
 *   
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE  * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER   
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE * SOFTWARE. 
 * 
 */

/** File class.
  * This class encapsulates a C file. 
  * @author David Lapsley (dlapsley@haystack.mit.edu)
  */
/** 
  */

#ifndef FILE_H
#define FILE_H

#include <common.h>
#include <string>
#include <iostream>
#ifdef LINUX
#include <unistd.h>
#endif // LINUX
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <Object.h>

using namespace std;

/** File class.
  * This class encapsulates a C file. I wanted a way to do non-blocking I/O
  * but could not find one using C++. So this class has been designed to
  * do that. It can switch between blocking and non-blocking using the
  * set_blocking() method. Everything else is reasonably straightforward.
  * @author David Lapsley (dlapsley@haystack.mit.edu)
  */
class File: public Object {
protected:
	/** File descriptor.
	  * This member encapsulates the file descriptor.
	  */
    int _fd;
	/** Blocking flag.
	  * This indicated whether or not the File object is in blocking mode
	  * or not.
	  */
	int _blocking;
	/** Blocking flags.
	  * These are the flags returned from fcntl() prior to setting a File
	  * non-blocking. They are stored in this member so that they can be 
	  * restored if a request to go back to blocking mode is made.
	  */
	int _blocking_flags;
public:
	/** Constant.
	  * This constant is used to indicate that a new File object is
	  * is for reading (in).
	  */
	static const int in;
	/** Constant.
	  * This constant is used to indicate that a new File object is
	  * is for writing (out).
	  */
	static const int out;
	/** Constant.
	  * This constant is used to indicate the blocking mode of operation.
	  */
	static const int BLOCKING;
	/** Constant.
	  * This constant is used to indicate the nonblocking mode of operation.
	  */
	static const int NONBLOCKING;
	/** Constructor.
	  * The constructor opens the file with the supplied name and opens
	  * it in the appropriate mode (File::in, File::out).
	  * @param name The name of the file to be open.
	  * @param mode The mode to use in opening the file.
	  */
    File(const string& name, const int& mode);
	/** Destructor. */
	virtual ~File();
	/** I/O method
	  * Reads data from the file into the supplied buffer.
	  * @param b Buffer to store data in.
	  * @param n Number of bytes to attempt to read.
	  * @return Number of bytes read.
	  */
	int read(char* b, const int& n);
	/** I/O method
	  * Writes data from the supplied buffer to the file.
	  * @param b Buffer to read data from.
	  * @param n Number of bytes to attempt to write.
	  * @return Number of bytes written.
	  */
	int write(const char* const b, const int& n);
	/** Blocking method.
	  * Sets the blocking mode of the file.
	  * @param b Blocking mode (File::blocking, File::nonblocking).	
	  */
	int set_blocking(const int& b);
	/** Blocking method.
	  * Get blocking mode.
	  * @return the blocking mode of the file.
	  */
	int get_blocking() const;
	/** File connection termination.
	  * Closes the file.
	  * @return 0: on success, -1 on failure.
	  */
    int close();
};

#endif


