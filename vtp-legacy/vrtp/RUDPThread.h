/*
 *  RUDPThread.h
 *  vrtp
 *
 *  Created by David Lapsley on Sun Mar 07 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Options.h>
#include <Thread.h>
#include <SocketBuffer.h>
#include <map>
#include <pthread.h>

#ifndef _RUDPTHREAD_H_
#define _RUDPTHREAD_H_

const unsigned int DEFAULT_MAX_WINDOW=64000;
const unsigned int DEFAULT_MTU=1024;

// XXXX
typedef map<unsigned int, SocketBuffer>::const_iterator ConstTxQueueIter;
typedef map<unsigned int, SocketBuffer>::iterator TxQueueIter;

class RUDPThread: public Thread
{
private:
    pthread_mutex_t _tx_queue_mutex;
    pthread_mutex_t _rx_queue_mutex;
    map<unsigned int, SocketBuffer> _tx_queue;
    map<unsigned int, SocketBuffer> _rx_queue;
    ConstTxQueueIter _curr_buf;
    unsigned int _curr_tx_seq_num;
    unsigned int _curr_rx_seq_num;

    unsigned int _curr_tx_ack_seq_num;
    unsigned int _curr_rx_ack_seq_num;
    unsigned int _max_window;
    unsigned int _curr_tx_window;
    unsigned int _mtu;
    struct RUDPSocketPacketHdr {
        unsigned char _type;
        unsigned char _synch;
    };
    enum RUDPSocketPacketHdrType {
        RUDPSocket_DATA,
        RUDPSocket_ACK
    };
public:
    RUDPThread(const string& id);
    ~RUDPThread();
    void run(Options o);
    void* thread_func();
    void proc();
    int send(const SocketBuffer& b);
    int recv(SocketBuffer& b);
    //
    unsigned int get_tx_seq_num() const;
    unsigned int get_ack_seq_num() const;
    unsigned int get_max_window() const;
    ConstTxQueueIter get_first_tx_iter();
    ConstTxQueueIter get_next_tx_iter();
    ConstTxQueueIter get_end_tx_iter();
    void test(Options o);
};



#endif // _RUDPTHREAD_H_


