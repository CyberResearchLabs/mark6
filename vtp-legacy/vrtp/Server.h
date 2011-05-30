/*
 *  Server.h
 *  vrtp
 *
 *  Created by David Lapsley on Wed Feb 25 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Options.h>
#include <Thread.h>
#include <string>

#ifndef _SERVER_H_
#define _SERVER_H_

using namespace std;

class Server: public Thread {
private:
    string _local_ip;
    u_int16_t _local_port;
    string _remote_ip;
    u_int16_t _remote_port;
    string _from_file;
    int _max_buf;
    int _streams;
    int _protocol;
public:
    Server(string &id);
    virtual ~Server();
    void run(Options o);
    void* thread_func();
    void vsie_file_source();
    void vsie_m5_source();
    void bit_source();
    void test(void* opts);
};

#endif // _SERVER_H_

