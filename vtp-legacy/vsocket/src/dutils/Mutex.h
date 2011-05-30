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


#ifndef MUTEX_H
#define MUTEX_H

#include <common.h>
#include <pthread.h>

/** Guard class.
  * Encapsulates a pthread_mutex_t. Construction locks the mutex,
  * destructrion unlocks it. Provides a nice way to save accidentally
  * locking a mutex and returning from a method without releasing it.
  * Note that this class encapsulates an externally created mutex.
  * @author David Lapsley (dlapsley@haystack.mit.edu)
  */
class Guard {
	/** Encapsulated mutex.
	  * Pointer to encapsulated mutex.
	  */
	pthread_mutex_t* _mutex;
public:
	/** Constructor.
	  * The constructor sets the _mutex member and locks it.
	  * @param Mutex to be encapsulated (note that it is assumed that the
	  * mutex has already been initialized.
	  */
	Guard(pthread_mutex_t* m);
	/** Destructor.
	  * This unlocks the mutex and then returns.
	  */
	~Guard();
};

/** Mutex class.
  * This is similar to the Guard class, except that it encapsulates its
  * own mutex. This mutex is created and destroyed by the Mutex class.
  * @author David Lapsley (dlapsley@haystack.mit.edu)
  */
class Mutex {
	/** Mutex object.
	  * This is the mutex object itself that is used to provide thread-safe
	  * -ness.
	  */
	pthread_mutex_t _mutex;
public:
	/** Constructor.
	  * Constructs the mutex object.
	  */
	Mutex();
	/** Destructor.
	  * Destroyes the mutex object.
	  */
	~Mutex();
	/** Lock mutex.
	  * This method locks the mutex.
	  */
	void lock();
	/** Unlock mutex.
	  * This method unlocks the mutex.
	  */
	void unlock();
};

#endif // MUTEX_H

