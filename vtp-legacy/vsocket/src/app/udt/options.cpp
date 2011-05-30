#ifdef LINUX
#include <unistd.h>
#endif // LINUX

#include <iostream>
#include "options.h"

Options::Options(int argc, char* const argv[]){    
	char opt;    
	opterr = 0;    
	if (argc<2) {        
		Options::usage(cerr);    
		exit(1);
	}    
	while ((opt=getopt(argc, (char*const*)argv, "p:P:i:I:F:T:L:b:D:n:r:m:M:S:R:")) != -1){
        switch (opt){
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
            case 'm':
                /* MTU */
                _mtu = atoi(optarg);
                break;
            case 'M':
                /* Mem threshold */
                _mem_thresh = atoi(optarg);
                break;
            case 'S':
                /* Socket MTU */
                _sock_mtu = atoi(optarg);
                break;
			case 'R':
				_rate=atoi(optarg);
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
    c << "Usage:    -i <local ip>" << endl;
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
    c << "          -m <mtu>" << endl;
    c << "          -M <mem threshold>" << endl;

}

string Options::get_remote_ip() const
{
    return(_remote_ip);
}

int Options::get_remote_port() const
{
    return(_remote_port);
}   
    
string Options::get_local_ip() const
{   
    return(_local_ip);
}   
    
int Options::get_local_port() const
{   
    return(_local_port);
}   

int Options::get_mtu() const
{   
    return(_mtu);
}   

int Options::get_sock_mtu() const
{   
    return(_sock_mtu);
}   

int Options::get_mem_thresh() const
{   
    return(_mem_thresh);
}   
    
string Options::get_from_file() const
{
    return(_from_file);
}
    
string Options::get_to_file() const
{
    return(_to_file);
}

string Options::get_log_file() const
{
    return(_log_file);
}

int Options::get_max_buf() const
{
    return(_max_buf);
}

int Options::get_test() const
{
    return(_test);
}

int Options::get_debug_level() const
{
    return(_debug_level);
}

int Options::get_streams() const
{
    return(_streams);
}

int Options::get_protocol() const
{
    return(_protocol);
}

int Options::get_rate() const
{
    return(_rate);
}
