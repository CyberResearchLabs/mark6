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


#include "Logger.h"

Logger::Logger(Mutex& m):
_mutex(m)
{
    _enabled=false;
    _terminal=false;
    _max_string=1024;
}  // End logger().

void Logger::set_stream(string& name)
{
    _mutex.lock();
    _out.close();
    _out.open(name.c_str(), ios_base::out | ios_base::app);
    _mutex.unlock();
}  // End set_stream().

void Logger::close_stream()
{
    _mutex.lock();
    _out.close();
    _mutex.unlock();
}  // End set_stream().

void Logger::set_terminal(bool &t)
{
    _mutex.lock();
    _terminal=true;
    _mutex.unlock();
}  // End set_terminal().

void Logger::enable_logging()
{
    _mutex.lock();
    _enabled=true;
    _mutex.unlock();
}  // End enable_logging().

void Logger::disable_logging()
{
    _mutex.lock();
    _enabled=false;
    _mutex.unlock();
}  // End disable_logging().

void Logger::log_info(ostringstream& ss)
{
    _mutex.lock();
    if (!_enabled) {
        _mutex.unlock();
        return;
    }
    if (_out.is_open()) {
        _out << ss.str() << endl;
    } else {
        cout << ss.str() << endl;
    }
    ((stringbuf*)ss.rdbuf())->str("\0");        
    _mutex.unlock();
} // End log_info().

Logger& Logger::operator<<(const string& ss)
{
	if (!_enabled) {
		return(*this);
	}
    _mutex.lock();
    if (_out.is_open()) {
        _out << ss;
    } else {
        cerr << ss;
    }   
    _mutex.unlock();
    if (_terminal)
        exit(1);
    return(*this);
}  // End operator<<().

Logger& Logger::operator<<(const int& nn)
{
	if (!_enabled) {
		return(*this);
	}
    _mutex.lock();
    ostringstream ss;
    ss << nn;
    
    if (_out.is_open()) {
        _out << ss.str();
    } else {
        cerr << ss.str();
		cerr.flush();
    }   
    _mutex.unlock();
    if (_terminal)
        exit(1);
    return(*this);
}  // End operator<<().

Logger& Logger::operator<<(const unsigned int& nn)
{
	if (!_enabled) {
		return(*this);
	}
    _mutex.lock();
    ostringstream ss;
    ss << nn;
    
    if (_out.is_open()) {
        _out << ss.str();
    } else {
        cerr << ss.str();
		cerr.flush();
    }   
    _mutex.unlock();
    if (_terminal)
        exit(1);
    return(*this);
}  // End operator<<().

Logger& Logger::operator<<(const double& nn)
{
	if (!_enabled) {
		return(*this);
	}
    _mutex.lock();
    ostringstream ss;
    ss << nn;
    
    if (_out.is_open()) {
        _out << ss.str();
    } else {
        cerr << ss.str();
		cerr.flush();
    }   
    _mutex.unlock();
    if (_terminal)
        exit(1);
    return(*this);
}  // End operator<<().

Logger& Logger::operator<<(const long& nn)
{
	if (!_enabled) {
		return(*this);
	}
    _mutex.lock();
    ostringstream ss;
    ss << nn;
    
    if (_out.is_open()) {
        _out << ss.str();
    } else {
        cerr << ss.str();
		cerr.flush();
    }   
    _mutex.unlock();
    if (_terminal)
        exit(1);
    return(*this);
}  // End operator<<().

Logger& Logger::operator<<(const char& cc)
{
	if (!_enabled) {
		return(*this);
	}
    _mutex.lock();
    ostringstream ss;
    ss << cc;
    
    if (_out.is_open()) {
        _out << ss.str();
    } else {
        cerr << ss.str();
		cerr.flush();
    }   
    _mutex.unlock();
    if (_terminal)
        exit(1);
    return(*this);
}  // End operator<<().


