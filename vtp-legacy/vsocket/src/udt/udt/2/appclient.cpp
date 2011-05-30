#include <iostream>
#include <udt.h>
#include <tcpsocket.h>
#include <string>
#include <iostream>
#include <utils.h>
#include <options.h>
#include <fstream>
#include <cstdlib>

int main(int argc, char** argv)
{
	Options o(argc, argv);

	CUDT* client = new CUDT;
	TCPSocket out_sock;

	const string local_ip(o.get_local_ip());
	const int local_port(o.get_local_port());
	const string remote_ip(o.get_remote_ip());
	const int remote_port(o.get_remote_port());
	const int mtu(o.get_mtu());
	const int sock_mtu(o.get_sock_mtu());

    cerr << "Remote IP:   " << remote_ip << endl;
    cerr << "Local IP:    " << local_ip << endl;
    cerr << "Remote port: " << remote_port << endl;
    cerr << "Local port:  " << local_port << endl;
    cerr << "MTU:         " << mtu << endl;
    cerr << "Sock MTU:    " << sock_mtu << endl;

#ifndef SOCKET
	ofstream out_file("output.dat", ios::binary | ios::out | ios::trunc);
#endif

	Timer total_time, loop_time;
	int total_bytes_sent=0, total_bytes_rcvd=0;
	int bytes_sent=0, bytes_rcvd=0;
	int loops=0;
	double recv_rate_sum=0;
	double alpha=0.5;
	char data[mtu];
	try  {
#ifdef SOCKET
		out_sock.connect(remote_ip, remote_port);
		cerr << "Connected to rx m5\n";
#endif
		client->open(local_port);
		client->listen();
		cerr << "Connected to incoming stream\n";
		cerr << "About to enter receive loop\n";
    	total_time.start();
		while (1) {
			loop_time.start();
			bytes_rcvd=client->recv(data, mtu);
        	double receive_rate = (double)bytes_rcvd/loop_time.elapsed();
			loop_time.stop();
        	// cerr << "rcvd: " << bytes_rcvd << endl;
        	// cerr << "tbr:  " << total_bytes_rcvd << endl;
			total_bytes_rcvd+=bytes_rcvd;
			// recv_rate_sum=alpha*receive_rate+(1-alpha)*recv_rate_sum;
			recv_rate_sum+=receive_rate;
        	// cerr << "bytes_rcvd:         " << bytes_rcvd << endl;
        	// cerr << "elapsed(s):         " << loop_time.elapsed() << endl;
        	// cerr << "receive_rate(bps):  " << receive_rate << endl;
			int s=0;
#ifdef SOCKET
			int bytes_left=bytes_rcvd;
			while (bytes_left>0) {
				bytes_sent=out_sock.send(data, bytes_left);
				if (bytes_sent==-1)
					break;
				bytes_left-=bytes_sent;
			}
#else
			out_file.write(data, bytes_rcvd);
#endif
			total_bytes_sent+=bytes_sent;
			loops++;
		}
	} catch (CUDTException e) {
		cerr << " some problem: " << e.getErrorMessage() << "\n";
		out_sock.shutdown(SHUT_RDWR);
#ifdef SOCKET
		out_sock.close();
#else
		out_file.close();
#endif
		total_time.stop();
	    cerr << "total_bytes_rcvd:    " << total_bytes_rcvd << endl;
	    cerr << "total_bytes_sent:    " << total_bytes_sent << endl;
	    cerr << "total_time(s):       " << total_time.elapsed() << endl;
    	cerr << "receive_rate(bps):   " 
			<< 8.0*(double)recv_rate_sum/loops
            << endl;
    	cerr << "avg receive_rate(bps):" 
			<< 8.0*(double)recv_rate_sum
            << endl;

	}
#ifdef SOCKET
	out_sock.shutdown(SHUT_RDWR);
	out_sock.shutdown(SHUT_RDWR);
	out_sock.close();
#else
	out_file.close();
#endif
	client->close();
	return 1;
}
