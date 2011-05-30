/*
 *  RUDPReceiver.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sun Mar 07 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "RUDPReceiver.h"

RUDPReceiver::RUDPReceiver(RUDPSocket* conn, const string& id):
Thread(id)
{
    debug_logger() << "RUDPReceiver::RUDPReceiver()\n";
    debug_logger() << "-RUDPReceiver::RUDPReceiver()\n";
}

RUDPReceiver::~RUDPReceiver()
{
    debug_logger() << "RUDPReceiver::~RUDPReceiver()\n";
    debug_logger() << "-RUDPReceiver::~RUDPReceiver()\n";    
}

void RUDPReceiver::run(Options o)
{
    debug_logger() << "RUDPReceiver::run()\n";
    debug_logger() << "-RUDPReceiver::run()\n";        
}

void* RUDPReceiver::thread_func()
{
    debug_logger() << "RUDPReceiver::thread_func()\n";
    debug_logger() << "-RUDPReceiver::thread_func()\n";
    return(NULL);
}

void RUDPReceiver::recv_proc()
{
    debug_logger() << "RUDPReceiver::recv_proc()\n";
    debug_logger() << "-RUDPReceiver::recv_proc()\n";    
}

void RUDPReceiver::test(Options o)
{
    debug_logger() << "RUDPReceiver::test()\n";
    debug_logger() << "-RUDPReceiver::test()\n";    
}
