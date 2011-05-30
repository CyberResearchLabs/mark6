/*
 *  RUDPSender.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sun Mar 07 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "RUDPSender.h"

RUDPSender::RUDPSender(RUDPSocket* conn, const string& id):
Thread(id)
{
    debug_logger() << "RUDPSender::RUDPSender()\n";
    debug_logger() << "-RUDPSender::RUDPSender()\n";
}

RUDPSender::~RUDPSender()
{
    debug_logger() << "RUDPSender::~RUDPSender()\n";
    debug_logger() << "-RUDPSender::~RUDPSender()\n";    
}

void RUDPSender::run(Options o)
{
    debug_logger() << "RUDPSender::run()\n";
    debug_logger() << "-RUDPSender::run()\n";        
}

void* RUDPSender::thread_func()
{
    debug_logger() << "RUDPSender::thread_func()\n";
    debug_logger() << "-RUDPSender::thread_func()\n";
    return(NULL);
}

void RUDPSender::send_proc()
{
    debug_logger() << "RUDPSender::send_proc()\n";
    debug_logger() << "-RUDPSender::send_proc()\n";    
}

void RUDPSender::test(Options o)
{
    debug_logger() << "RUDPSender::test()\n";
    debug_logger() << "-RUDPSender::test()\n";    
}
