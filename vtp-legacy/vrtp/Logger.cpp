/*
 *  Logger.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "Logger.h"

Logger::Logger()
{
    _enabled=false;
    _terminal=false;
    _max_string=1024;
    pthread_mutex_init(&_mutex, NULL);
}  // End logger().

void Logger::lock_mutex()
{
    if (pthread_mutex_lock(&_mutex)!=0) {
        cerr << "    Logger::lock_mutex(): Unable to get lock" << endl;
        exit(1);
    }
}

void Logger::unlock_mutex()
{
    if (pthread_mutex_unlock(&_mutex)!=0) {
        cerr << "    Logger::unlock_mutex(): Unable to release lock" << endl;
        exit(1);
    }
}

void Logger::set_stream(ofstream& out, string& name)
{
    lock_mutex();
    _out.close();
    _out.open(name.c_str(), ios_base::out | ios_base::app);
    unlock_mutex();
}  // End set_stream().

void Logger::enable_logging()
{
    lock_mutex();
    _enabled=true;
    unlock_mutex();
}  // End enable_logging().

void Logger::disable_logging()
{
    lock_mutex();
    _enabled=false;
    unlock_mutex();
}  // End disable_logging().

void Logger::log_info(ostringstream& ss)
{
    lock_mutex();
    if (!_enabled) {
        unlock_mutex();
        return;
    }
    if (_out.is_open()) {
        _out << ss.str() << endl;
    } else {
        cout << ss.str() << endl;
    }
    ((stringbuf*)ss.rdbuf())->str("\0");        
    unlock_mutex();
} // End log_info().

Logger& Logger::operator<<(const string& ss)
{
    lock_mutex();
    if (!_enabled) {
        unlock_mutex();
        if (_terminal)
            exit(1);
        return(*this);
    }
    if (_out.is_open()) {
        _out << ss;
    } else {
        cerr << ss;
    }   
    unlock_mutex();
    if (_terminal)
        exit(1);
    return(*this);
}  // End operator<<().

Logger& Logger::operator<<(const int& nn)
{
    lock_mutex();
    if (!_enabled) {
        unlock_mutex();
        if (_terminal)
            exit(1);
        return(*this);
    }
    ostringstream ss;
    ss << nn;
    
    if (_out.is_open()) {
        _out << ss.str();
    } else {
        cerr << ss.str();
    }   
    unlock_mutex();
    if (_terminal)
        exit(1);
    return(*this);
}  // End operator<<().

Logger& Logger::operator<<(const unsigned int& nn)
{
    lock_mutex();
    if (!_enabled) {
        unlock_mutex();
        if (_terminal)
            exit(1);
        return(*this);
    }
    ostringstream ss;
    ss << nn;
    
    if (_out.is_open()) {
        _out << ss.str();
    } else {
        cerr << ss.str();
    }   
    unlock_mutex();
    if (_terminal)
        exit(1);
    return(*this);
}  // End operator<<().

Logger& Logger::operator<<(const double& nn)
{
    lock_mutex();
    if (!_enabled) {
        unlock_mutex();
        if (_terminal)
            exit(1);
        return(*this);
    }
    ostringstream ss;
    ss << nn;
    
    if (_out.is_open()) {
        _out << ss.str();
    } else {
        cerr << ss.str();
    }   
    unlock_mutex();
    if (_terminal)
        exit(1);
    return(*this);
}  // End operator<<().

Logger& Logger::operator<<(const long& nn)
{
    lock_mutex();
    if (!_enabled) {
        unlock_mutex();
        if (_terminal)
            exit(1);
        return(*this);
    }
    ostringstream ss;
    ss << nn;
    
    if (_out.is_open()) {
        _out << ss.str();
    } else {
        cerr << ss.str();
    }   
    unlock_mutex();
    if (_terminal)
        exit(1);
    return(*this);
}  // End operator<<().

Logger& Logger::operator<<(const char& cc)
{
    lock_mutex();
    if (!_enabled) {
        unlock_mutex();
        if (_terminal)
            exit(1);
        return(*this);
    }
    ostringstream ss;
    ss << cc;
    
    if (_out.is_open()) {
        _out << ss.str();
    } else {
        cerr << ss.str();
    }   
    unlock_mutex();
    if (_terminal)
        exit(1);
    return(*this);
}  // End operator<<().


