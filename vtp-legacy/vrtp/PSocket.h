/*
 *  PSocket.h
 *  vrtp
 *
 *  Created by David Lapsley on Fri Mar 19 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */


#include <list>
#include <Socket.h>
#include <TCPSocket.h>
#include <SocketBuffer.h>
#include <stdexcept>

#ifndef _PSOCKET_H_
#define _PSOCKET_H_

class PSocketError: public runtime_error {
public:
    PSocketError(const string& msg): runtime_error(msg) { }
};

const int DEFAULT_PSOCKET_BUFFER_SIZE=9000;

class PSocket: public Socket 
{
    list<TCPSocket> _sock_list;
    list<TCPSocket>::iterator _sock_list_iter;
    list<TCPSocket> _accept_sock_list;
    list<TCPSocket>::iterator _accept_sock_list_iter;
    int _num_ports;
    int _mtu;
    string _buf;
public:
    PSocket(int num_ports, int mtu);
    PSocket(const PSocket &s);
    virtual ~PSocket();
    PSocket& operator=(const PSocket& s);
    const int get_mtu() const;
    
    // Socket API.
    void bind(string ip, int port);
    void listen();
    void accept();
    void connect(string ip, int port);
    int recv(SocketBuffer& s);
    int recv(string& s, const int n);
    int send(const SocketBuffer& s);
    int send(const string& s, const int n);
    void close();
    int shutdown(int howto);
#ifdef _DAVE_DEBUG
    virtual int test(void*);
    virtual int dump(void*);
#endif
};

#endif

