
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
	int _rate;
public: 
        Options(int argc, char* const argv[]);
    ~Options();
    void usage(ostream& c);
    string get_remote_ip() const;
    int get_remote_port() const;
    string get_local_ip() const;
    int get_local_port() const;
	int get_mtu() const;
	int get_sock_mtu() const;
	int get_mem_thresh() const;
    string get_log_file() const;
    string get_from_file() const;
    string get_to_file() const;
    int get_max_buf() const;
    int get_test() const;
    int get_debug_level() const;
    int get_streams() const;
    int get_protocol() const;
    int get_rate() const;
};


#endif // _OPTIONS_H_
