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


#ifndef LOGSYSTEM_H
#define LOGSYSTEM_H


#include <common.h>
#include <Logger.h>
#include <pthread.h>
#include <Mutex.h>

using namespace std;

/** Logsystem class.
  * This provides a high level encapsulation of a complete logging system 
  * for an application. It is collection of singletons (a quad-ton?).
  * It contains for separate static logger singleton objects. Each one 
  * of these is used to provide logging for a particular "debug" level.
  * Currently there are four levels: debug, info, warning, error.
  * Logsystem uses a "double guard" to provide thread safe access to 
  * each of these instances while minimizing latency due to excessive
  * mutex locking. 
  * @author David Lapsley (dlapsley@haystack.mit.edu)
  */
class Logsystem {
	/** Class mutex.
	  * Used to ensure thread-safe access to each of the singletons.
	  */
    static Mutex _mutex;
	/** Instance variable.
	  * Instance variable for supporting the debug level.
	  */
    static Logger* _debug_logger_instance;
	/** Instance variable.
	  * Instance variable for supporting the info level.
	  */
    static Logger* _info_logger_instance;
	/** Instance variable.
	  * Instance variable for supporting the warning level.
	  */
    static Logger* _warning_logger_instance;
	/** Instance variable.
	  * Instance variable for supporting the error level.
	  */
    static Logger* _error_logger_instance;
protected:
	/** Constructor.
	  * Never called publicly.
	  */
    Logsystem();
public:
	/** Instance method.
	  * Provides access to the underlying Logger singleton object.
	  * @return pointer to static debug_logger object.
	  */
    static Logger* debug_logger();
	/** Instance method.
	  * Provides access to the underlying Logger singleton object.
	  * @return pointer to static info_logger object.
	  */
    static Logger* info_logger();
	/** Instance method.
	  * Provides access to the underlying Logger singleton object.
	  * @return pointer to static warning_logger object.
	  */
    static Logger* warning_logger();
	/** Instance method.
	  * Provides access to the underlying Logger singleton object.
	  * @return pointer to static error_logger object.
	  */
    static Logger* error_logger();
};

#endif // LOGSYSTEM_H

