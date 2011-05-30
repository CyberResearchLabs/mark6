/*
 *  Client.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Wed Feb 25 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <TCPSocket.h>
#include <UDPSocket.h>
#include <iostream.h>
#include <fstream.h>

#include "Client.h"

Client::Client()
{
    
}

Client::~Client()
{
    
}

void Client::run(Options o) {
    clog << "Client::run()" << endl;
    try {
        TCPSocket client;
        client.connect(o.get_remote_ip(), o.get_remote_port());
        const int MAX_BUF=1024;
        SocketBuffer buf(MAX_BUF);
        ofstream output(o.get_to_file().c_str(), ios_base::out | ios_base::trunc);
        int bytes_rcvd=0;
                
        while ((bytes_rcvd=client.recv(buf))>0) {
            buf.set_size(bytes_rcvd);
            int bytes_wrtn=output.write((char*)buf, bytes_rcvd);
            cout << "    Bytes Rcvd: " << bytes_rcvd << endl;
            cout << "    Bytes Written: " << bytes_wrtn << endl;
        }
                
        client.close();
        output.close();
    } catch (exception e) {
        clog << "Client::run - exception." << endl;
    }
}

void Client::test(Options o)
{
    cout << "run_client()" << endl;
    TCPSocket client;
    client.connect(o.get_remote_ip(), o.get_remote_port());
    const int MAX_BUF=1024;
    SocketBuffer buf(MAX_BUF);
    for (int i=0; i<100; i++) {
        sprintf(buf, "hello %d", i);
        client.send(buf);
    }
    usleep(1000000);
    client.close();
    
    UDPSocket udp_server;
    udp_server.bind("0.0.0.0", 49998);
    for (int i=0; i<100; i++) {
        udp_server.recvfrom("127.0.0.1", 49999, buf);
        cout << buf << endl;
    }
    udp_server.close();
}


