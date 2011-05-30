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


/*
 * This software is released under the terms of the MIT License(included below).
 * 
 * Copyright (c) 2004 MIT Haystack Observatory
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a 
 * copy of this software and associated documentation files (the "Software"), 
 * to deal in the Software without restriction, including without limitation 
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, 
 * and/or sell copies 
 * of the Software, and to permit persons to whom the Software is furnished to 
 * do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in 
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER  
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */

#ifndef LOGGER_H
#define LOGGER_H

#include <common.h>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <pthread.h>
#include <Mutex.h>

using namespace std;

/** Logger class.
  * @doc This class provides logging support. It is thread safe and uses
  * a mutex to control access to the members. Logging can be enabled and
  * disabled completely. operator<< is overloaded to allow ease of
  * use in the calling context. There is a slight performance penalty
  * associated with using this class (one function call + a conditional)
  * even when it is disabled. For nearly all applications this should be 
  * negligible, however, for time critical applications, this could be an 
  * issue.
  * @author David Lapsley (dlapsley@haystack.mit.edu)
  */
class Logger {
protected:
	/** @doc All messages are sent to this stream. */
    ofstream _out;
	/** @doc This flag controls whether or not messages are
	  * printed to the ofstream. 
	  */
    bool _enabled;
	/** @doc Used to control access to data members from multiple threads */
	Mutex& _mutex;
	/** @doc If set, the exit() will be called at the end of the logging
	  * function.
	  */
    bool _terminal;
	/** @doc Maximum length of the string. */
    int _max_string;
	/** @doc Currently not used. */
    string _prefix;
public:
	/** Constructor.
	  * Sets flags to false and the max string to 1024.
	  */
    Logger(Mutex& m);
	/** Set output stream.
	  * This allows the output stream to be set to allow the calling context
	  * to determine where log messages should be directed to.
	  * @param out output stream that log messages will be directed to.
	  * @param name name of the output sream
	  */
    void set_stream(string &name);
	/** Close output stream.
	  * This allows the output stream to be closed.
	  */
    void close_stream();
	/** Set terminal flag.
	  * If the terminal flag is enabled, then any call to a method that 
	  * logs a message will result in a call to exit(1) after the message
	  * has been logged.
	  * @param t value to set terminal to.
	  */
    void set_terminal(bool &t);
	/** Enable logging function.
	  * @doc Enable logging function. The logger will now log messages to 
	  * the file supplied. 
	  * @param name the name of the output stream.
	  */
    void enable_logging();
	/** Disable logging function.
	  * @doc Disable logging function. The logger will not log messages to 
	  * _out_stream. Instead, all loggin methods will return immediately
	  * without doing any logging.
	  */
    void disable_logging();
	/** Log an ostringstream.
	  * @deprecated This is no longer used. Use the overloaded operator<<
	  * instead.
	  */
    void log_info(ostringstream& ss);
	/** Log string.
	  * @doc Log supplied string.
	  * @param ss String to be logged.
	  * @return Reference to this Logger object.
	  */
    Logger& operator<<(const string& ss);
	/** Log integer.
	  * @doc Log supplied integer.
	  * @param ss integer to be logged.
	  * @return Reference to this Logger object.
	  */
    Logger& operator<<(const int& nn);
	/** Log unsigned integer.
	  * @doc Log supplied unsigned integer.
	  * @param ss unsigned integer to be logged.
	  * @return Reference to this Logger object.
	  */
    Logger& operator<<(const unsigned int& nn);
	/** Log double.
	  * @doc Log supplied double.
	  * @param ss double to be logged.
	  * @return Reference to this Logger object.
	  */
    Logger& operator<<(const double& nn);
	/** Log long.
	  * @doc Log supplied long.
	  * @param ss long to be logged.
	  * @return Reference to this Logger object.
	  */
    Logger& operator<<(const long& nn);
	/** Log char.
	  * @doc Log supplied char.
	  * @param ss char to be logged.
	  * @return Reference to this Logger object.
	  */
    Logger& operator<<(const char& cc);
};

#endif // LOGGER_H


