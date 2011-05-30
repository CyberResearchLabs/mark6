/*
 *  Test.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Logger.h>
#include <Thread.h>
#include <time.h>
#include <stdlib.h>
#include <RTPIsStream.h>
#include <RTPOsStream.h>

#include "Test.h"

Test::Test()
{
    debug_logger() << "Test::Test()\n";
    debug_logger() << "-Test::Test()\n";    
}  // End Test().

Test::~Test()
{
    debug_logger() << "Test::~Test()\n";
    debug_logger() << "-Test::~Test()\n";    
}  // End ~Test().

void Test::run(Options o)
{
    int seed=100;
    srand(seed);
    
    debug_logger() << "Test::run()\n";
#if 0
    msg << "    Thread test...";
    log_info(msg);
    
    string s1("Thread1"), s2("Thread2"), s3("Thread3"), s4("Thread4");
    Thread t1(s1);
    Thread t2(s2);
    Thread t3(s3);
    Thread t4(s4);
    
    t1.thread_create();
    t2.thread_create();
    t3.thread_create();
    t4.thread_create();
    
    t4.join(); 
    t3.join();
    t2.join();
    t1.join();
#endif
    SocketBuffer s(1024);
    Timestamp t;
    string ip;
    unsigned int port;
	const int mtu=1400;
    if (o.get_test()==1) {
        ip=o.get_local_ip();
        port=o.get_local_port();
        RTPIsStream is(ip, port, mtu);
        int rcvd_bytes=0;
        while ((rcvd_bytes=is.read_next_sample(s, t))>0) {
            debug_logger() << " bytes rcvd=" << rcvd_bytes << " data=" 
                << ((char*)s)[0] << "\n";
        }
    } else if (o.get_test()==2) {
        ip=o.get_remote_ip();
        port=o.get_remote_port();
        RTPOsStream os(ip, port, mtu);
        int i=0;
        for (i=0; i<1023; i++) {
            ((char*)s)[i]='a';
        }
        ((char*)s)[i]='\0';
        s.set_size(1024);
        int sent_bytes=0;
        while ((sent_bytes=os.write_next_sample(s, t))>0) {
            debug_logger() << " bytes sent=" << sent_bytes << "\n";
        }
    } else if (o.get_test()==3) {
        int i;
        ip=o.get_local_ip();
        port=o.get_local_port();
        PSocket p(8, 1400);
        p.bind(ip, port);
        p.listen();
        p.accept();
        for (i=0; i<10; i++) {
            p.recv(s);
            cout << s << endl;
        }
    } else if (o.get_test()==4) {
        int i;
        ip=o.get_remote_ip();
        port=o.get_remote_port();
        PSocket p(8, 1400);
        p.connect(ip, port);
        for (i=0; i<1024; i++)
            (char*)s[i]='a'+((char)i)%26;
        for (i=0; i<10; i++) {
            p.send(s);
            cout << s << endl;
        }
    }
    
    debug_logger() << "-Test::run()\n";    
}  // End run().

