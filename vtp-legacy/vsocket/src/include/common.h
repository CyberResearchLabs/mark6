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


#ifndef COMMON_H
#define COMMON_H

#ifdef HAVE_CONFIG
#include <config.h>
#endif // HAVE_CONFIG

#ifdef HAVE_SOCKLEN_T 
        typedef int socklen_t;
#endif  // HAVE_SOCKLEN_T

#define ENDL "\n"

// Messages at start and end of functions/methods.
#define XSTART_FUNC(X, Y) \
X::debug_logger() << "DEBUG:   " << Y << "::" << __func__ << "\n";

#define XEND_FUNC(X, Y)   \
X::debug_logger() << "DEBUG:   " << Y << "::" << "-" << __func__ << "\n";

#define DEBUG_START_FUNC
#define DEBUG_END_FUNC
#define START_FUNC(X) debug_logger() << "DEBUG:   " << X << "::" << __func__ << "\n";
#define END_FUNC(X)   debug_logger() << "DEBUG:   " << X << "::" << "-" << __func__ << "\n";

// Function/method messages.
#define XDEBUG_FUNC(X, Y) \
X::debug_logger() <<   "DEBUG:   " << Y << "::" << __func__ << ": "
#define XINFO_FUNC(X, Y)	\
X::info_logger() <<    "INFO:    " << Y << "::" << __func__ << ": "
#define XWARNING_FUNC(X, Y) \
X::warning_logger() << "WARNING: " << Y << "::" << __func__ << ": "
#define XERROR_FUNC(X, Y) \
X::error_logger() <<   "ERROR:   " << Y << "::" << __func__ << ": "

#define DEBUG_FUNC(X)   debug_logger() <<   "DEBUG:   " << X << "::" << __func__ << ": "
#define INFO_FUNC(X)	 info_logger() <<    "INFO:    " << X << "::" << __func__ << ": "
#define WARNING_FUNC(X) warning_logger() << "WARNING: " << X << "::" << __func__ << ": "
#define ERROR_FUNC(X)	 error_logger() <<   "ERROR:   " << X << "::" << __func__ << ": "

// Default constants.
#define DEFAULT_MTU 1024
#define DEFAULT_PEAK_RATE 100000000
#define DEFAULT_WINDOW_SIZE 1000

enum Mode {
	FILE2NET,
	IN2NET,
	NET2FILE,
	NET2OUT,
	TVG2NET,
        NET2TVG
};

#endif // COMMON_H
