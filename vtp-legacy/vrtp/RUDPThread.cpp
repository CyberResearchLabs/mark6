/*
 *  RUDPThread.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sun Mar 07 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "RUDPThread.h"

RUDPThread::RUDPThread(const string& id):
Thread(id)
{
    debug_logger() << "RUDPThread::RUDPThread()\n";
    _max_window=DEFAULT_MAX_WINDOW;
    _curr_tx_window=_max_window;
    pthread_mutex_init(&_tx_queue_mutex, NULL);
    pthread_mutex_init(&_rx_queue_mutex, NULL);
    debug_logger() << "-RUDPThread::RUDPThread()\n";
}

RUDPThread::~RUDPThread()
{
    debug_logger() << "RUDPThread::~RUDPThread()\n";
    pthread_mutex_destroy(&_tx_queue_mutex);
    pthread_mutex_destroy(&_rx_queue_mutex);
    debug_logger() << "-RUDPThread::~RUDPThread()\n";    
}

void RUDPThread::run(Options o)
{
    debug_logger() << "RUDPThread::run()\n";
    
    // Launch the separate thread. This will result in Server::thread_func()
    // (defined below) being launched in its own thread of execution.
    Thread::thread_create();
    
    debug_logger() << "-RUDPThread::run()\n";        
}

void* RUDPThread::thread_func()
{
    debug_logger() << "RUDPThread::thread_func()\n";
    // Call the function that does stuff.
    proc();
    debug_logger() << "-RUDPThread::thread_func()\n";
    return(NULL);
}

void RUDPThread::proc()
{
    debug_logger() << "RUDPThread::send_proc()\n";
    
    
    debug_logger() << "-RUDPThread::send_proc()\n";    
}

int RUDPThread::send(const SocketBuffer& b)
{
    debug_logger() << "RUDPThread::send()\n";
    pthread_mutex_lock(&_tx_queue_mutex);
    if (_tx_queue.size()>=_max_window) {
        return(-1);
    }
    _tx_queue[_curr_tx_seq_num++]=b;
    pthread_mutex_unlock(&_tx_queue_mutex);
    debug_logger() << "RUDPThread::-send()\n";    
}

int RUDPThread::recv(SocketBuffer& b)
{
    debug_logger() << "RUDPThread::recv()\n";
    pthread_mutex_lock(&_rx_queue_mutex);
    if (_rx_queue.size()==0)
        return(0);
    b=_rx_queue[_curr_rx_seq_num];
    _rx_queue.erase(_curr_rx_seq_num++);
    pthread_mutex_unlock(&_rx_queue_mutex);
    debug_logger() << "RUDPThread::-recv()\n";    
}

unsigned int RUDPThread::get_tx_seq_num() const
{
    debug_logger() << "RUDPThread::get_tx_seq_num()\n";
    debug_logger() << "-RUDPThread::get_tx_seq_num()\n";
    return(_curr_tx_seq_num);
}

unsigned int RUDPThread::get_ack_seq_num() const
{
    debug_logger() << "RUDPThread::get_ack_seq_num()\n";
    debug_logger() << "-RUDPThread::get_ack_seq_num()\n";
#if 0
    return(_curr_ack_seq_num);
#endif 
    return(0);
    // XXXX
}

unsigned int RUDPThread::get_max_window() const
{
    debug_logger() << "RUDPThread::get_max_window()\n";
    debug_logger() << "-RUDPThread::get_max_window()\n";
    return(_max_window);
}

ConstTxQueueIter RUDPThread::get_first_tx_iter()
{
    debug_logger() << "RUDPThread::get_first_tx_iter()\n";
    _curr_buf=_tx_queue.begin();
    debug_logger() << "-RUDPThread::get_first_tx_iter()\n";
    return(_curr_buf);
}

ConstTxQueueIter RUDPThread::get_next_tx_iter()
{
    debug_logger() << "RUDPThread::get_next_tx_iter()\n";
    ++_curr_buf;
    debug_logger() << "-RUDPThread::get_next_tx_iter()\n";
    return(_curr_buf);
}

ConstTxQueueIter RUDPThread::get_end_tx_iter()
{
    debug_logger() << "RUDPThread::get_end_tx_iter()\n";
    _curr_buf=_tx_queue.end();
    debug_logger() << "-RUDPThread::get_end_tx_iter()\n";
    return(_curr_buf);
}

void RUDPThread::test(Options o)
{
    debug_logger() << "RUDPThread::test()\n";
    debug_logger() << "-RUDPThread::test()\n";    
}
