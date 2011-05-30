
#include <string>

#ifndef _OPTIONS_H_
#define _OPTIONS_H_

using namespace std;

class Options {
private:
    string _remote_ip;
    int _remote_port;
    string _local_ip;
    int _local_port;
	int _mtu;
	int _sock_mtu;
	int _mem_thresh;
    string _log_file;
    string _from_file;
    string _to_file;
    int _max_buf;
    int _test;
    int _debug_level;
    int _streams;
    int _protocol;
public: 
        Options(int argc, char* const argv[]);
    ~Options();
    void usage(ostream& c);
    string get_remote_ip();
    int get_remote_port();
    string get_local_ip();
    int get_local_port();
	int get_mtu();
	int get_sock_mtu();
	int get_mem_thresh();
    string get_log_file();
    string get_from_file();
    string get_to_file();
    int get_max_buf();
    int get_test();
    int get_debug_level();
    int get_streams();
    int get_protocol();
};


#endif // _OPTIONS_H_
