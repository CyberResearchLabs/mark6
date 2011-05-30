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


#include "Logsystem.h"

Mutex Logsystem::_mutex;

Logger* Logsystem::_debug_logger_instance=0;
Logger* Logsystem::_info_logger_instance=0;
Logger* Logsystem::_warning_logger_instance=0;
Logger* Logsystem::_error_logger_instance=0;

Logger* Logsystem::debug_logger()
{
	if (_debug_logger_instance==0) {
    	_mutex.lock();
    	if (_debug_logger_instance==0) {
        	_debug_logger_instance=new Logger(_mutex);
    	}        
    	_mutex.unlock();
	}
    return(_debug_logger_instance);
}

Logger* Logsystem::info_logger()
{
	if (_info_logger_instance==0) {
    	_mutex.lock();
    	if (_info_logger_instance==0) {
        	_info_logger_instance=new Logger(_mutex);
    	}
    	_mutex.unlock();
	}
    return(_info_logger_instance);
}

Logger* Logsystem::warning_logger()
{
	if (_warning_logger_instance==0) {
    	_mutex.lock();
    	if (_warning_logger_instance==0) {
        	_warning_logger_instance=new Logger(_mutex);
    	}
    	_mutex.unlock();    
	}
    return(_warning_logger_instance);}

Logger* Logsystem::error_logger()
{
	if (_error_logger_instance==0) {
    	_mutex.lock();    
    	if (_error_logger_instance==0) {
        	_error_logger_instance=new Logger(_mutex);
    	}
    	_mutex.unlock();
	}
    return(_error_logger_instance);
}


