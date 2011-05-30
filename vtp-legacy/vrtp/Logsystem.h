/*
 *  Logsytem.h
 *  vrtp
 *
 *  Created by David Lapsley on Tue Mar 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Logger.h>
#include <pthread.h>

#ifndef _LOGSYSTEM_H_
#define _LOGSYSTEM_H_

using namespace std;

class Logsystem {
    static pthread_mutex_t _mutex;
    static Logger* _debug_logger_instance;
    static Logger* _info_logger_instance;
    static Logger* _warning_logger_instance;
    static Logger* _error_logger_instance;
protected:
    Logsystem();
public:
    static Logger* debug_logger();
    static Logger* info_logger();
    static Logger* warning_logger();
    static Logger* error_logger();
};

#endif // _LOGSYSTEM_H_



