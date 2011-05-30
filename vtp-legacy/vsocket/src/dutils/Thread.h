/*
 *  Thread.h
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef THREAD_H
#define THREAD_H

#include <common.h>
#include <iostream>
#include <pthread.h>
#include <Object.h>

/** Thread class.
  * This class encapsulates a POSIX pthread. In order to use this class,
  * inherit from it and override thread_func(). To spawn the thread,
  * first create the Thread-derived object, then call thread_create(). 
  * The thread will be crated, thread_func() will run inside the thread
  * and the thread id will be stored in the _thread_id data member.
  * A call to join() will pause the calling thread until the thread exits.
  * @author David Lapsley (dlapsley@haystack.mit.edu)
  */
class Thread: public Object {
protected:
	/** Thread identifier.
	  * @doc This stores the value of the thread spawned after calling 
	  * thread_create().
	  */
    pthread_t _thread_id;
	/** Thread identifier.
	  * This holds a descriptive string that can be used by the thread to
	  * identifiy itself.
	  */
    string _id;
public:
	/** Thread body.
	  * This function should be overridden by inheriting classes. This function
	  * gets called by the system within the new thread, after thread_create()
	  * is called. In order to pass information into the thread, simply 
	  * add data members to the inheriting class and fill these in with
	  * appropriate information prior to calling thread_create(). thread_func()
	  * will be able to access this data through *this.
	  * @return Returns a pointer to any object that needs to be passed back
	  * once the thread has terminated. 
	   */
    virtual void* thread_func();
	/** Constructor.
	  * Default constructor.
	  */
    Thread();
	/** Constructor.
	  * The descriptive id is set to the supplied parameter.
	  * @param s Descriptive string id.
	  */
    Thread(const string& s);
	/** Destructor.
	  * Since this is intended to be a base class, the destructor is virtual.
	  */
    virtual ~Thread();
	/** Thread creation.
	  * This method calls pthread_create() to create a thread which will 
	  * execute thread_funct() as its body.
	  */
    void thread_create();
	/** Join thread.
	  * Pause calling context until the thread has completed.
	  */
    void join();
	/** Test.
	  * This method is for tests suites and debugging.
	  */
    virtual void test(void* args);
};

#endif // THREAD_H

