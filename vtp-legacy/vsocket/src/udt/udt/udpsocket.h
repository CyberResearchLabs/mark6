/*
 *  UDPSocket.h
 *  vrtp
 *
 *  Created by David Lapsley on Tue Feb 24 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */


#include <list>
#include <string>

using namespace std;

#ifndef _UDP_SOCKET_H_
#define _UDP_SOCKET_H_

const int UDP_DEFAULT_BUFFER_SIZE=9000;

class UDPSocket
{
    int _sockd;
public:
    UDPSocket();
    UDPSocket(int s);
    ~UDPSocket();
    
    // Socket API.
    void bind(const string& ip, const int& port);
    void connect(const string& ip, const int& port);
	int recv(char* s, const int& n);
	int send(const char*s, const int& n);
    int recvfrom(const string& ip, const int& port, const char* s, const int&n );
    int sendto(const string& ip, const int& port, const char* s, const int& n);
    // int recvfrom(string ip, int port, SocketBuffer& s);
    // int sendto(string ip, int port, SocketBuffer& s);
    void close();
};

#endif

