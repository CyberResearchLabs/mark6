#include <iostream>
#include <udt.h>
#include <tcpsocket.h>
#include <unistd.h>
#include <utils.h>
#include <options.h>
#include <fstream>
#include <cstdlib>



int main(int argc, char** argv)
{
	Options o(argc, argv);

   	CUDT* server = new CUDT;
	// bool boolval=false;
	// server->setOpt(UDT_SNDSYN, &boolval, sizeof(bool));
   	TCPSocket accept_sock, in_sock;

   	string local_ip=string(o.get_local_ip());
   	string remote_ip=string(o.get_remote_ip());
   	int local_port(o.get_local_port());
	int remote_port(o.get_remote_port());
	const int mtu(o.get_mtu());
	const int mem_thresh(o.get_mem_thresh());
	const int sock_mtu(o.get_sock_mtu());
	
	cerr << "Remote IP:   " << remote_ip << endl;
	cerr << "Local IP:    " << local_ip << endl;
	cerr << "Remote port: " << remote_port << endl;
	cerr << "Local port:  " << local_port << endl;
	cerr << "mtu:         " << mtu << endl;
	cerr << "mem_thresh:  " << mem_thresh << endl;

#ifndef SOCKET
	ifstream in_file("input.dat", ios::binary | ios::in);
#endif

	try
	{

		server->open();
		server->connect(remote_ip.c_str(), remote_port);
		cerr << "server connected to client\n";

#if SOCKET
   		accept_sock.bind(local_ip, local_port);

   		accept_sock.listen();
		int s = accept_sock.accept();
		if (s==-1)
			cerr << "Invalid accept\n";
   		in_sock.set_sockd(s);
#endif
   		cerr << "Server received connection from incoming Mark5\n";
	}
	catch(CUDTException e)
	{
		cerr << "error msg: " << e.getErrorMessage() << endl;
		return 0;
	}
   	char* data;
    int total_bytes_sent=0, total_bytes_rcvd=0;
    int bytes_sent=0, bytes_rcvd=0;
	cerr << "about to rcv\n";
	Timer total_time, loop_time;
	double send_time=0;
	total_time.start();
   	do {
		data=new char[mtu];
#ifdef SOCKET
		bytes_rcvd=in_sock.recv(data, mtu);
#else
		in_file.read(data, mtu);
		bytes_rcvd=in_file.gcount();
#endif
		if (bytes_rcvd<=0)
			break;
		// cerr << "rcvd: " << bytes_rcvd << endl;
		// cerr << "tbr:  " << total_bytes_rcvd << endl;
		total_bytes_rcvd+=bytes_rcvd;
		loop_time.start();
		bytes_sent=server->send(data, bytes_rcvd);
		total_bytes_sent+=bytes_sent;
		while (server->getCurrSndBufSize() > mem_thresh)
			usleep(1);
		loop_time.stop();
		send_time+=loop_time.elapsed();
		// double transmit_rate = (double)bytes_sent/loop_time.elapsed();
		// cerr << "bytes_sent:         " << bytes_sent << endl;
        // cerr << "bytes_rcvd:         " << bytes_rcvd << endl;
		// cerr << "elapsed(s):          " << loop_time.elapsed() << endl;
		// cerr << "transmit_rate(bps): " << transmit_rate << endl;
	} while (bytes_rcvd>0);
	in_file.close();
	usleep(1000);
	while (server->getCurrSndBufSize() > mem_thresh)
		usleep(100);
	total_time.stop();
		cerr << "total_bytes_sent:    " << total_bytes_sent << endl;
        cerr << "total_bytes_rcvd:    " << total_bytes_rcvd << endl;
	cerr << "total_time(s):       " << total_time.elapsed() << endl;	
	cerr << "transmit_rate:  (bps)" 
		<< 8.0*(double)total_bytes_sent/total_time.elapsed()
		<< endl;
	cerr << "transmit_rate:  (bps)" 
		<< 8.0*(double)total_bytes_sent/send_time
		<< endl;
	server->close();
	accept_sock.shutdown(SHUT_RDWR);
	accept_sock.close();

	return 1;
}
