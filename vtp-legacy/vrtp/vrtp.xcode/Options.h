/*
 *  Options.h
 *  vrtp
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <string>

#ifndef _OPTIONS_H_
#define _OPTIONS_H_

using namespace std;

enum Mode {
    CLIENT,
    SERVER,
    TEST
};

class Options {
private:
    Mode _mode;
    string _remote_ip;
    int _remote_port;
    string _local_ip;
    int _local_port;
    string _log_file;
    string _from_file;
    string _to_file;
public:
        Options(int argc, char* const argv[]);
    ~Options();
    void usage(ostream& c);
    Mode get_mode();
    string get_remote_ip();
    int get_remote_port();
    string get_local_ip();
    int get_local_port();
    string get_log_file();
    string get_from_file();
    string get_to_file();
};


#endif
