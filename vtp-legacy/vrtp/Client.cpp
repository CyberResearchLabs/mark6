/*
 *  Client.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Wed Feb 25 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */
#ifdef LINUX
#include <unistd.h>
#endif // LINUX

#include <Utils.h>
#include <TCPSocket.h>
#include <UDPSocket.h>
#include <iostream>
#include <fstream>
#include <SocketBuffer.h>
#include <RTPIsStream.h>
#include <RTPOsStream.h>
#include <TestOfStream.h>
#include <TestIfStream.h>
#include <RTPIPsockStream.h>

#include "Client.h"

Client::Client(string &id):
Thread(id)
{
    debug_logger() << "Client::Client()\n";
    debug_logger() << "-Client::Client()\n";
}

Client::~Client()
{
    debug_logger() << "Client::~Client()\n";
    debug_logger() << "-Client::~Client()\n";
}

void Client::run(Options o) {
    debug_logger() << "Client::run()\n";
    // Do any options processing here. Parse options structure and put
    // information into class. This can then be accessed by the separate
    // thread of execution later on.
    _remote_ip=o.get_remote_ip();
    _remote_port=o.get_remote_port();
    _local_ip=o.get_local_ip();
    _local_port=o.get_local_port();
    _to_file=o.get_to_file();
    _max_buf=o.get_max_buf();
    _streams=o.get_streams();
    _protocol=o.get_protocol();
    // Launch the separate thread. This will result in Client::thread_func()
    // (defined below) being launched in its own thread of execution.
    Thread::thread_create();
    debug_logger() << "-Client::run()\n";
}

void* Client::thread_func()
{
    debug_logger() << "Client::thread_func()\n";   
    debug_logger() << "    id: " << _id << "\n";
    switch (_protocol) {
        case 0:
            vsie_file_sink();
            break;
        case 1:
            vsie_m5_sink();
            break;
    }
    debug_logger() << "-Client::thread_func()\n";   
    return(NULL);
}

void Client::vsie_m5_sink()
{
    debug_logger() << "Client::vsie_m5_sink()\n";
    try {
        // Constants.
        const string m5_ip(_remote_ip);
        const int m5_port(_remote_port);
        const int mtu=1024;
        // XXXX
        TCPSocket cmd;
        string op("net2disc=open:test1.dat;\n");
        string cl("net2disc=close;\n");
        string res;

        cmd.connect(_remote_ip, 2620);
        
        usleep(1000000);
        
        debug_logger() << "    OP size:  " << (int)op.size() << "\n";
        cout << "    OP string: ";
        cout.write(op.c_str(), 25);
        cout << "\n";
        
        // cmd.send(op);    THIS WORKS!
        //cmd.send(op, op.size());
        //cmd.recv(res, 100);
        //info_logger() << "    Response: " << res << "\n";
        //usleep(1000000);
        
        // Connect to receiving m5.
        RTPOsStream disc_os(m5_ip, m5_port, mtu);
        // TestOfStream disc_os(_to_file);
        info_logger() << "Client:: connected to disc_os\n";
        
        // Wait for incoming connection from transmitting m5.
        // RTPIsStream net_is(_local_ip, _local_port, mtu);
        RTPIPsockStream net_is(_local_ip, _local_port, mtu, _streams);
        // string in_file("input.dat");
        // TestIfStream net_is(in_file);
        info_logger() << "Client:: connected to net_is\n";
        
        // Counters.
        int bytes_rcvd=0;
        int bytes_wrtn=0;
    
#if 0
        SocketBuffer buf(1024);
        Timestamp t;
        Timer w;
        w.start();
        long total_bytes_wrtn=0;
        long total_bytes_rcvd=0;
        buf.set_req_size(mtu);
        while ((bytes_rcvd=net_is.read_next_sample(buf, t))>0) {
            buf.set_req_size(bytes_rcvd);   // Must set_req for write_next_sample().
            total_bytes_rcvd+=bytes_rcvd;
            debug_logger() << "   bytes_rcvd==" << bytes_rcvd << "\n";
            debug_logger() << "   buffer size==" << buf.get_size() << "\n";
            debug_logger() << "    tbrx: " << total_bytes_rcvd << "\n";
            if ((bytes_wrtn=disc_os.write_next_sample(buf, t))<=0) {
                debug_logger() << "disc_os.send() zero bytes written\n";
                break;
            }
            total_bytes_wrtn+=bytes_wrtn;
            debug_logger() << "    tbr: " << total_bytes_wrtn << "\n";
            buf.clear();
            buf.set_req_size(mtu);
        }
#else
        string buf;
        Timestamp t;
        Timer w;
        w.start();
        long total_bytes_wrtn=0;
        long total_bytes_rcvd=0;
        while ((bytes_rcvd=net_is.read_next_sample(buf, 1024, t))>0) {
            total_bytes_rcvd+=bytes_rcvd;
            debug_logger() << "   bytes_rcvd==" << bytes_rcvd << "\n";
            debug_logger() << "   buffer size==" << (int)buf.size() << "\n";
            debug_logger() << "    tbrx: " << total_bytes_rcvd << "\n";
            if ((bytes_wrtn=disc_os.write_next_sample(buf, bytes_rcvd, t))<=0) {
                debug_logger() << "disc_os.send() zero bytes written\n";
                break;
            }
            total_bytes_wrtn+=bytes_wrtn;
            debug_logger() << "    tbr: " << total_bytes_wrtn << "\n";
        }
#endif
        w.stop();
        double rate=double(total_bytes_wrtn)/w.elapsed();
        info_logger() << "    Elapsed time: " << w.elapsed() << "\n";
        info_logger() << "    Bytes written:" << total_bytes_wrtn << "\n";
        info_logger() << "    Rate:         " << rate << " Bps\n";
        info_logger() << "    Rate:         " << 8.0*rate << " bps\n";
        debug_logger() << "-Client::vsie_m5_sink()\n";
        
        //cmd.send(cl, cl.size());
        cmd.shutdown(SHUT_RDWR);
        cmd.close();
        //usleep(5000000);        
    } catch (exception e) {
        warning_logger() << "    Exception caught! " << e.what() << "\n";
    }

}

void Client::vsie_file_sink()
{
    debug_logger() << "Client::vsie_file_sink()\n";

    // Constants.
    const int mtu=1024;
    
    // Connect to receiving m5.
    TestOfStream disc_os(_to_file);
    debug_logger() << "Client:: connected to disc_os\n";
    
    // Wait for incoming connection from transmitting m5.
    RTPIPsockStream net_is(_local_ip, _local_port, mtu, _streams);
    debug_logger() << "Client:: connected to net_is\n";
    
    // Counters.
    int bytes_rcvd=0;
    int bytes_wrtn=0;
    
    SocketBuffer buf(1024);
    Timestamp t;
    
    Timer w;
    w.start();
    long total_bytes_wrtn=0;
    while ((bytes_rcvd=net_is.read_next_sample(buf, t))>0) {
        debug_logger() << "bytes_rcvd==" << bytes_rcvd << "\n";
        if ((bytes_wrtn=disc_os.write_next_sample(buf, t))<=0) {
            debug_logger() << "disc_os.send() zero bytes written\n";
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
    debug_logger() << "-Client::vsie_file_sink()\n";
}

void Client::bit_sink()
{
    debug_logger() << "Client::bit_sink()\n";
    debug_logger() << "    id: " << _id << "\n";
    
    try {
        TCPSocket client;
        client.connect(_remote_ip, _remote_port);
        const int MAX_BUF=1024;
        SocketBuffer buf(MAX_BUF);
        ofstream output(_to_file.c_str(), ios_base::out | ios_base::trunc);
        ostringstream msg(ostringstream::out | ostringstream::trunc);
        int bytes_rcvd=0;
        
        while ((bytes_rcvd=client.recv(buf))>0) {
            buf.set_size(bytes_rcvd);
            output.write((char*)buf, bytes_rcvd);
            int bytes_wrtn=bytes_rcvd;
            debug_logger() << "    Bytes Rcvd: " << bytes_rcvd;
            debug_logger() << "    Bytes Written: " << bytes_wrtn;
        }
        client.close();
        output.close();
    } catch (exception e) {
        clog << "Client::bit_sink - exception." << endl;
    }
    debug_logger() << "-Client::bit_sink()\n";
}

void Client::test(Options o)
{
    debug_logger() << "Client::test\n";
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
    debug_logger() << "-Client::test\n";
}


