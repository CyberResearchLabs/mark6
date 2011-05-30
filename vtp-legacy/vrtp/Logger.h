/*
 *  Logger.h
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <pthread.h>

#ifndef _LOGGER_H_
#define _LOGGER_H_

using namespace std;

class Logger {
protected:
    ofstream _out;
    bool _enabled;
    pthread_mutex_t _mutex;
    bool _terminal;
    int _max_string;
    string _prefix;
public:
    Logger();
    void lock_mutex();
    void unlock_mutex();
    void set_stream(ofstream& out, string &name);
    void enable_logging();
    void disable_logging();
    void log_info(ostringstream& ss);
    Logger& operator<<(const string& ss);
    Logger& operator<<(const int& nn);
    Logger& operator<<(const unsigned int& nn);
    Logger& operator<<(const double& nn);
    Logger& operator<<(const long& nn);
    Logger& operator<<(const char& cc);
};

#endif // _LOGGER_H_


