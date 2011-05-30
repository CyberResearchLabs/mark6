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


/* 
 * This software is released under the terms of the MIT License(included below). *  
 * Copyright (c) 2004 MIT Haystack Observatory 
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


#ifndef UTILS_H
#define UTILS_H

#include <common.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <string.h>
#include <sys/time.h>

/** Timer class.
  * This class is used to make it easier to measure the elapsed time between
  * two points in a program. It encapsulates 3 timevals which are used to store:
  * starting time, stop time and the difference between the start and stop
  * time.
  * @author David Lapsley (dlapsley@haystack.mit.edu)
  */
class Timer {
	/** @doc Stores the start time. */
    struct timeval _start;
	/** @doc Stores the stop time. */
    struct timeval _stop;
	/** @doc Stores the difference between the start and stop time. */
    struct timeval _diff;
public:
	/** Constructor. */
    Timer();
	/** Destructor. */
    ~Timer();
	/** Start method.
	  * Start the timer running (this just involves capturing the start time).
	  */
    inline void start() {
    	gettimeofday(&_start, NULL);
	}
	/** Stop method.
	  * Stop the timer running (this just involves capturing the stop time).
	  */
    inline void stop() {
    	gettimeofday(&_stop, NULL);
	}
	/** Elapsed time.
	  * Elapsed time.
	  * @return The amount of time that elapsed from the time the Timer was
	  * started, to the time it was stopped.
	  */
    inline double elapsed() {
		timersub(&_stop, &_start, &_diff);
    	double ret=_diff.tv_sec+_diff.tv_usec/1e6;
    	return(ret);
	}
};

#endif // UTILS_H

