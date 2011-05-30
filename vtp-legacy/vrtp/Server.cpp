/*
 *  Server.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Wed Feb 25 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#ifdef LINUX
#include <unistd.h>
#endif // LINUX

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <TCPSocket.h>
#include <UDPSocket.h>
#include <SocketBuffer.h>

#include <TestIfStream.h>
#include <TestOfStream.h>
#include <RTPIsStream.h>
#include <RTPOsStream.h>

#include <RTPOPsockStream.h>
#include <Utils.h>
#ifdef XXXX
#include <RTPOuStream.h>
#endif

#include "Server.h"

Server::Server(string& id):
Thread(id)
{
    debug_logger() << "Server::Server()\n";
    debug_logger() << "-Server::Server()\n";
}

Server::~Server()
{
    debug_logger() << "Server::~Server()\n";
    debug_logger() << "-Server::~Server()\n";
}

void Server::run(Options o)
{
    debug_logger() << "Server::run()\n";

    // Do any options processing here. Parse options structure and put
    // information into class. This can then be accessed by the separate
    // thread of execution later on.
    _local_ip=o.get_local_ip();
    _local_port=o.get_local_port();
    _remote_ip=o.get_remote_ip();
    _remote_port=o.get_remote_port();
    _from_file=o.get_from_file();
    _max_buf=o.get_max_buf();
    _streams=o.get_streams();
    _protocol=o.get_protocol();
    
    // Launch the separate thread. This will result in Server::thread_func()
    // (defined below) being launched in its own thread of execution.
    Thread::thread_create();

    debug_logger() << "-Server::run()\n";
}

void* Server::thread_func()
{
    debug_logger() << "Server::run()\n";
    debug_logger() << "    id: " << _id << "\n";
    switch (_protocol) {
        case 0:
            vsie_file_source();
            break;
        case 1:
            vsie_m5_source();
            break;
    }
    debug_logger() << "-Server::run()\n";
    return(NULL);
}

void Server::vsie_file_source()
{
    debug_logger() << "Server::vsie_file_source()\n";
    
    // Constants.
    const int mtu=1024;
    
    // Connect to receiving m5 client.
    RTPOPsockStream net_os(_remote_ip, _remote_port, mtu, _streams);
    debug_logger() << "    connected to client()\n";

    // Connect to incoming file.
    TestIfStream disc_is(_from_file);
    debug_logger() << "    connected to disc_is()\n";

    // Counters.
    int bytes_rcvd=0;
    int bytes_wrtn=0;
    
    SocketBuffer buf(1024);
    Timestamp t;
    Timer w;
    w.start();
    long total_bytes_wrtn=0;
    while ((bytes_rcvd=disc_is.read_next_sample(buf, t))>0) {
        debug_logger() << "    disc_is: bytes_rcvd==" << bytes_rcvd << "\n";
        if ((bytes_wrtn=net_os.write_next_sample(buf, t))<=0) {
            debug_logger() << "    net_os.send() zero bytes written" << "\n";
            break;
        }
        total_bytes_wrtn+=bytes_wrtn;
    }
    w.stop();
    double rate=double(total_bytes_wrtn)/w.elapsed();
    info_logger() << "    Elapsed time: " << w.elapsed() << "\n";
    info_logger() << "    Bytes written:" << total_bytes_wrtn << "\n";
    info_logger() << "    Rate:         " << rate << " Bps\n";
    info_logger() << "    Rate:         " << 8.0*rate << " bps\n";    
    debug_logger() << "-Server::vsie_file_source()\n";
}

void Server::vsie_m5_source()
{
    debug_logger() << "Server::vsie_m5_source()\n";
    
    // Constants.
    const string m5_ip(_local_ip);
    const int m5_port(_local_port);
    const int mtu=1024;
    
    // Connect to receiving m5 client.
    // string to_file("output.dat");
    // TestOfStream net_os(to_file);
    // RTPOsStream net_os(_remote_ip, _remote_port, mtu);
    RTPOPsockStream net_os(_remote_ip, _remote_port, mtu, _streams);
    debug_logger() << "    connected to client()\n";
    
    // Wait for transmitting m5 to connect.
    RTPIsStream disc_is(m5_ip, m5_port, mtu);
    // TestIfStream disc_is(_from_file);
    debug_logger() << "    connected to disc_is()\n";
    
    // Counters.
    int bytes_rcvd=0;
    int bytes_wrtn=0;
    
    try {
        string buf;
        Timestamp t;
        Timer w;
        w.start();
        long total_bytes_wrtn=0;
        long total_bytes_read=0;
        while ((bytes_rcvd=disc_is.read_next_sample(buf, 1024, t))>0) {
            total_bytes_read+=bytes_rcvd;
            if ((bytes_wrtn=net_os.write_next_sample(buf, bytes_rcvd, t))<=0) {
                debug_logger() << "    net_os.send() zero bytes written" << "\n";
                break;
            }
            total_bytes_wrtn+=bytes_wrtn;
            debug_logger() << "   disc_is: bytes_rcvd==" << bytes_rcvd << "\n";
            debug_logger() << "    tbrd: " << total_bytes_read << "\n";
            debug_logger() << "    tbw: " << total_bytes_wrtn << "\n";
        }
        w.stop();
        double rate=double(total_bytes_wrtn)/w.elapsed();
        info_logger() << "    Elapsed time: " << w.elapsed() << "\n";
        info_logger() << "    Bytes written:" << total_bytes_wrtn << "\n";
        info_logger() << "    Rate:         " << rate << " Bps\n";
        info_logger() << "    Rate:         " << 8.0*rate << " bps\n";    
        debug_logger() << "-Server::vsie_m5_source()\n";            
    } catch (exception e) {
        error_logger() << "   Caught exception! " << e.what() << "\n";
    }
}

void Server::bit_source()
{
    debug_logger() << "Server::bit_source()\n";
    debug_logger() << "    id: " << _id << "\n";
    
    try {
        TCPSocket server;
        server.bind(_local_ip, _local_port);
        server.listen();
        TCPSocket sock(server.accept());
        const int MAX_BUF=1024;
        SocketBuffer buf(MAX_BUF);
        ifstream input(_from_file.c_str(), ios_base::in | ios_base::binary);
        int bytes_read=0;
        do {
            input.read((char*)buf, MAX_BUF);
            bytes_read=input.gcount();
            buf.set_size(bytes_read);
            int bytes_sent=sock.send(buf);
            debug_logger() << "    Bytes Read: " << bytes_read;
            debug_logger() << "    Bytes Sent: " << bytes_sent;
        } while (bytes_read>0);
        sock.close();
        server.close();
        input.close();
    } catch (exception e) {
        cerr << "Server::bit_sink - exception." << endl;
    }
    debug_logger() << "    id: " << _id << " exiting...";
    debug_logger() << "-Server::bit_source()\n";
}


void Server::test(void* opts)
{
    Options o=*(Options*)opts;
    debug_logger() << "Server::test()\n";
    TCPSocket server;
    server.bind(o.get_local_ip(), o.get_local_port());
    server.listen();
    TCPSocket sock(server.accept());
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
    debug_logger() << "-Server::test()\n";
}

