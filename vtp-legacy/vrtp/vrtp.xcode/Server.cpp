/*
 *  Server.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Wed Feb 25 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <iostream.h>
#include <fstream>
#include <cstdlib>
#include <TCPSocket.h>
#include <UDPSocket.h>

#include "Server.h"

Server::Server()
{
    
}

Server::~Server()
{
    
}

void Server::run(Options o)
{
    clog << "Server::run()" << endl;
    try {
        TCPSocket server;
        server.bind(o.get_local_ip(), o.get_local_port());
        server.listen();
        TCPSocket sock=server.accept();
        const int MAX_BUF=1024;
        SocketBuffer buf(MAX_BUF);
        ifstream input(o.get_from_file().c_str());
        int bytes_read=0;
        while ((bytes_read=input.readsome((char*)buf, MAX_BUF))>0) {
            buf.set_size(bytes_read);
            int bytes_sent=sock.send(buf);
            cout << "    Bytes Read: " << bytes_read << endl;
            cout << "    Bytes Sent: " << bytes_sent << endl;
        }
        sock.close();
        server.close();
        input.close();
    } catch (exception e) {
        clog << "Server::run - exception." << endl;
    }
}

void Server::test(Options o)
{
    cout << "Server::run_test()" << endl;
    TCPSocket server;
    server.bind(o.get_local_ip(), o.get_local_port());
    server.listen();
    TCPSocket sock=server.accept();
    const int MAX_BUF=1024;
    SocketBuffer buf(MAX_BUF);
    while (sock.recv(buf)>0) {
        cout << "Buf: " << buf << endl;
    }
    sock.close();
    server.close();
    
    usleep(5000000);
    UDPSocket udp_client;
    udp_client.bind("0.0.0.0", 49999);
    for (int i=0; i<100; i++) {
        sprintf(buf, "udp hello %d", i);
        udp_client.sendto("127.0.0.1", 49998, buf);
        usleep(100000);
    }
    udp_client.close();
    clog << "    leaving..." << endl;    
}
