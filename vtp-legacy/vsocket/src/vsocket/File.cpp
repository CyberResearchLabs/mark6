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

#include <File.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <unistd.h>

const int File::in=0;
const int File::out=1;
const int File::BLOCKING=0;
const int File::NONBLOCKING=1;

// Constructors/Destructors.
File::File(const string& name, const int& mode)
{
	START_FUNC("File")
	_blocking=BLOCKING;
	int file_mode=S_IRUSR | S_IWUSR; 
	if (mode==in) {
		_fd=open(name.c_str(), O_RDONLY, file_mode);
	} else if (mode==out) {
		_fd=open(name.c_str(), O_WRONLY | O_TRUNC | O_CREAT | O_APPEND, 
				file_mode);
	}
	if (_fd==-1) {
		ERROR_FUNC("File") << "unable to open file\n";
	}
	END_FUNC("File")
}


File::~File()
{
	START_FUNC("File")
	if (_fd!=-1)
		::close(_fd);
	END_FUNC("File")
}


// Member access.
int File::read(char* b, const int& n)
{
	START_FUNC("File")
	if (_fd==-1) {
		ERROR_FUNC("File")	
			<< "File::read(): attempt to read unopened descriptor\n";
		return(-1);
	}
	END_FUNC("File")
	return(::read(_fd, b, n));
}


int File::write(const char* const b, const int& n)
{
	START_FUNC("File")
	if (_fd==-1) {
		ERROR_FUNC("File")  
			<< "     File::write(): attempt to write unopened descriptor\n";
		return(-1);
	}
	END_FUNC("File")
	return(::write(_fd, b, n));
}


// Blocking.
int File::set_blocking(const int& b)
{
	START_FUNC("File")
    int ret=0;
    if (_blocking==b)
        return(0);
	_blocking=b;
    if (b==NONBLOCKING) {
        _blocking_flags=fcntl(_fd, F_GETFL, 0);
        ret=fcntl(_fd, F_SETFL, O_NONBLOCK);
    }  
    if (b==BLOCKING) {
        ret=fcntl(_fd, F_SETFL, _blocking_flags);
    }
    if (ret!=-1)
        ret=0;
	END_FUNC("File")
    return(ret);
}


int File::get_blocking() const
{
	START_FUNC("File")
	END_FUNC("File")
	return(_blocking);
}


// File connection termination...
int File::close()
{
	START_FUNC("File")
	if (_fd==-1) {
		ERROR_FUNC("File") 	
			<< "File::close() attempte to close unopened descriptor.\n";
	}
	int ret=::close(_fd);
	if (ret==0)
		_fd=-1;
	END_FUNC("File")
	return(ret);
}


