/*
 *  Options.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#ifdef LINUX
#include <unistd.h>
#endif // LINUX

#include <iostream>
#include "Options.h"


Options::Options(int argc, char* const argv[])
{
    char opt;
    opterr = 0;
    if (argc<2) {
        Options::usage(cerr);
    }
    while ((opt=getopt(argc, (char*const*)argv, "cst:p:P:i:I:F:T:L:b:D:n:r:")) != -1){
        switch (opt){
            case 'c':
                /* We are in client mode */
                _mode = CLIENT;
                break;
            case 's':
                /* We are in server mode */
                _mode = SERVER;
                break;
            case 't':
                /* We are in test mode */
                _mode = TEST;
                /* Test option */
                _test = atoi(optarg);
                break;
            case 'p':
                /* Control port */
                _local_port = atoi(optarg);
                break;
            case 'P':
                /* Data port */
                _remote_port = atoi(optarg);
                break;
            case 'i':
                /* IP */
                _local_ip=string(optarg);
                break;
            case 'I':
                /* File name */
                _remote_ip=string(optarg);
                break;
            case 'F':
                /* File name */
                _from_file=string(optarg);
                break;
            case 'T':
                /* File name */
                _to_file=string(optarg);
                break;
            case 'L':
                /* File name */
                _log_file=string(optarg);
                break;
            case 'b':
                /* Control port */
                _max_buf = atoi(optarg);
                break;
            case 'D':
                /* Debug level */
                _debug_level = atoi(optarg);
                break;
            case 'n':
                /* Num streams */
                _streams = atoi(optarg);
                break;
            case 'r':
                /* Transport protocol */
                _protocol = atoi(optarg);
                break;
            case '?':
                cerr << "Unknown option" << endl;
                exit(1);
        }
    }    
}

Options::~Options()
{
    
}

void Options::usage(ostream& c) {
    c << "Usage:    -c" << endl;
    c << "          -s" << endl;
    c << "          -t <test arg>" << endl;
    c << "          -i <local ip>" << endl;
    c << "          -I <remote ip>" << endl;
    c << "          -p <local port>" << endl;
    c << "          -P <remote port>" << endl;
    c << "          -F <from file>" << endl;
    c << "          -T <to file>" << endl;
    c << "          -L <log file>" << endl;
    c << "          -b <max buf size>" << endl;
    c << "          -D <debug level>" << endl; 
    c << "          -n <num streams>" << endl; 
    c << "          -r <transport protocol>" << endl; 

}
Mode Options::get_mode() {
    return(_mode);
}

string Options::get_remote_ip()
{
    return(_remote_ip);
}

int Options::get_remote_port()
{
    return(_remote_port);
}

string Options::get_local_ip()
{
    return(_local_ip);
}

int Options::get_local_port()
{
    return(_local_port);
}

string Options::get_from_file()
{
    return(_from_file);
}

string Options::get_to_file()
{
    return(_to_file);
}

string Options::get_log_file()
{
    return(_log_file);
}

int Options::get_max_buf()
{
    return(_max_buf);
}

int Options::get_test()
{
    return(_test);
}

int Options::get_debug_level()
{
    return(_debug_level);
}

int Options::get_streams()
{
    return(_streams);
}

int Options::get_protocol()
{
    return(_protocol);
}