/*
 *  Logsytem.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Tue Mar 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "Logsystem.h"

pthread_mutex_t Logsystem::_mutex=PTHREAD_MUTEX_INITIALIZER;

Logger* Logsystem::_debug_logger_instance=0;
Logger* Logsystem::_info_logger_instance=0;
Logger* Logsystem::_warning_logger_instance=0;
Logger* Logsystem::_error_logger_instance=0;

Logger* Logsystem::debug_logger()
{
    pthread_mutex_lock(&Logsystem::_mutex);
    if (_debug_logger_instance==0) {
        _debug_logger_instance=new Logger;
    }        
    pthread_mutex_unlock(&Logsystem::Logsystem::_mutex);
    return(_debug_logger_instance);
}

Logger* Logsystem::info_logger()
{
    pthread_mutex_lock(&Logsystem::_mutex);
    if (_info_logger_instance==0) {
        _info_logger_instance=new Logger;
    }
    pthread_mutex_unlock(&Logsystem::Logsystem::_mutex);
    return(_info_logger_instance);
}

Logger* Logsystem::warning_logger()
{
    pthread_mutex_lock(&Logsystem::_mutex);
    if (_warning_logger_instance==0) {
        _warning_logger_instance=new Logger;
    }
    pthread_mutex_unlock(&Logsystem::Logsystem::_mutex);    
    return(_warning_logger_instance);}

Logger* Logsystem::error_logger()
{
    pthread_mutex_lock(&Logsystem::_mutex);    
    if (_error_logger_instance==0) {
        _error_logger_instance=new Logger;
    }
    pthread_mutex_unlock(&Logsystem::Logsystem::_mutex);
    return(_error_logger_instance);
}

