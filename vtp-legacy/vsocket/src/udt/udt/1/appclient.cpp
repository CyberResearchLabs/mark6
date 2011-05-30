#include <iostream>
#include <udt.h>
#include <tcpsocket.h>
#include <string>
#include <iostream>
#include <utils.h>
#include <options.h>

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

	out_sock.connect(remote_ip, remote_port);
	cerr << "Connected to rx m5\n";

	Timer total_time, loop_time;
	int total_bytes_sent=0, total_bytes_rcvd=0;
	int bytes_sent=0, bytes_rcvd=0;
	int loops=0;
	double recv_rate_sum=0;
	double alpha=0.5;
	try  {
		client->open(local_port);
		client->listen();
		cerr << "Connected to incoming stream\n";
		char data[mtu], *read_ptr;
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
				read_ptr=data;
				int bytes_left=bytes_rcvd;
				for (int i=0; i<mtu/sock_mtu; i++) {
					int bytes_to_send=(bytes_left>sock_mtu)?sock_mtu:bytes_left;
					s=out_sock.send(read_ptr, bytes_to_send);
					if (s==-1)
						break;
					read_ptr+=s;
					bytes_left-=s;
					bytes_sent+=s;
				}
				total_bytes_sent+=bytes_sent;
				loops++;
		}
	} catch (CUDTException e) {
		cerr << " some problem: " << e.getErrorMessage() << "\n";
		out_sock.shutdown(SHUT_RDWR);
		out_sock.close();
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
	out_sock.shutdown(SHUT_RDWR);
	client->close();
	out_sock.shutdown(SHUT_RDWR);
	out_sock.close();
return 1;
}
