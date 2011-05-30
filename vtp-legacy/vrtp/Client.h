/*
 *  Client.h
 *  vrtp
 *
 *  Created by David Lapsley on Wed Feb 25 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Thread.h>
#include <Options.h>
#include <string>

#ifndef _CLIENT_H_
#define _CLIENT_H_

using namespace std;

class Client: public Thread {
private:
    string _remote_ip;
    u_int16_t _remote_port;
    string _local_ip;
    u_int16_t _local_port;
    string _to_file;
    int _max_buf;
    int _streams;
    int _protocol;
public:
    Client(string &id);
    ~Client();
    void run(Options o);
    void* thread_func();
    void vsie_file_sink();
    void vsie_m5_sink();
    void bit_sink();
    void test(Options o);
};

#endif // _CLIENT_H_

