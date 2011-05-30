#include <iostream>
#include <udt.h>
#include <tcpsocket.h>
#include <udpsocket.h>
#include <unistd.h>
#include <utils.h>
#include <options.h>
#include <fstream>
#include <cstdlib>
#include <time.h>

const int DEFAULT_MAX_SOCK_BUFFER=9000;

char* data, *write_ptr, *in_data, *out_data;

void udt_sock(const Options& o);
void udt_file(const Options& o);
void udp_sock(const Options& o);
void udp_sock2(const Options& o);
void usleep_that_works(u_int64_t usec);

int 
main(int argc, char** argv)
{
	Options o(argc, argv);

	switch (o.get_protocol()) {
	case 1:
		udt_sock(o);
		break;
	case 2:
		udt_file(o);
		break;
	case 3:
		udp_sock(o);
		break;
	case 4:
		udp_sock2(o);
		break;
	}
	return 1;
}

void udt_sock(const Options& o)
{
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

	try
	{
		server->open();
		server->connect(remote_ip.c_str(), remote_port);
		cerr << "server connected to client\n";

   		accept_sock.bind(local_ip, local_port);
   		accept_sock.listen();
		int s = accept_sock.accept();
		if (s==-1)
			cerr << "Invalid accept\n";
   		in_sock.set_sockd(s);
   		cerr << "Server received connection from incoming Mark5\n";
	}
	catch(CUDTException e)
	{
		cerr << "error msg: " << e.getErrorMessage() << endl;
	}
    int total_bytes_sent=0, total_bytes_rcvd=0;
    int bytes_sent=0, bytes_rcvd=0;
	cerr << "about to rcv\n";
	Timer total_time, loop_time;
	double send_time=0;
	total_time.start();
   	do {
		data=new char[mtu];
        write_ptr=data;
		bytes_rcvd=0;
        int r=0;
        for (int i=0; i<mtu/sock_mtu; i++) {
        	r=in_sock.recv(write_ptr, sock_mtu);
            write_ptr+=r;
            if (r<0)
            	break;
       		bytes_rcvd+=r;
       	}
		if (bytes_rcvd<=0)
			break;
		cerr << "rcvd: " << bytes_rcvd << endl;
		// cerr << "tbr:  " << total_bytes_rcvd << endl;
		total_bytes_rcvd+=bytes_rcvd;
		loop_time.start();
		bytes_sent=server->send(data, bytes_rcvd);
		total_bytes_sent+=bytes_sent;
		while (server->getCurrSndBufSize() > mem_thresh)
			usleep(10);
		loop_time.stop();
		send_time+=loop_time.elapsed();
		// double transmit_rate = (double)bytes_sent/loop_time.elapsed();
		// cerr << "bytes_sent:         " << bytes_sent << endl;
        // cerr << "bytes_rcvd:         " << bytes_rcvd << endl;
		// cerr << "elapsed(s):          " << loop_time.elapsed() << endl;
		// cerr << "transmit_rate(bps): " << transmit_rate << endl;
	} while (bytes_rcvd>0);
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
	in_sock.shutdown(SHUT_RDWR);
	in_sock.close();
	accept_sock.shutdown(SHUT_RDWR);
	accept_sock.close();
}

void udt_file(const Options& o)
{
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

	ifstream in_file("input.dat", ios::binary | ios::in);

	try
	{
		server->open();
		server->connect(remote_ip.c_str(), remote_port);
		cerr << "server connected to client\n";
   		cerr << "Server received connection from incoming Mark5\n";
	}
	catch(CUDTException e)
	{
		cerr << "error msg: " << e.getErrorMessage() << endl;
	}
   	char* data, *write_ptr;
    int total_bytes_sent=0, total_bytes_rcvd=0;
    int bytes_sent=0, bytes_rcvd=0;
	cerr << "about to rcv\n";
	Timer total_time, loop_time;
	double send_time=0;
	total_time.start();
   	do {
		data=new char[mtu];
        write_ptr=data;
		bytes_rcvd=0;
        int r=0;
        for (int i=0; i<mtu/sock_mtu; i++) {
        	r=in_sock.recv(write_ptr, sock_mtu);
            write_ptr+=r;
            if (r<0)
            	break;
       		bytes_rcvd+=r;
       	}
		in_file.read(data, mtu);
		bytes_rcvd=in_file.gcount();
		if (bytes_rcvd<=0)
			break;
		cerr << "rcvd: " << bytes_rcvd << endl;
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
	in_sock.shutdown(SHUT_RDWR);
	in_sock.close();
	accept_sock.shutdown(SHUT_RDWR);
	accept_sock.close();
}

void udp_sock(const Options& o)
{
   	TCPSocket accept_sock, in_sock;
	UDPSocket out_sock;

   	string local_ip=string(o.get_local_ip());
   	string remote_ip=string(o.get_remote_ip());
   	int local_port(o.get_local_port());
	int remote_port(o.get_remote_port());
	const int mtu(o.get_mtu());
	const int mem_thresh(o.get_mem_thresh());
	const int sock_mtu(o.get_sock_mtu());
	const int rate(o.get_rate());
	
	cerr << "Remote IP:   " << remote_ip << endl;
	cerr << "Local IP:    " << local_ip << endl;
	cerr << "Remote port: " << remote_port << endl;
	cerr << "Local port:  " << local_port << endl;
	cerr << "mtu:         " << mtu << endl;
	cerr << "sock_mtu:    " << sock_mtu << endl;

    int total_bytes_sent=0, total_bytes_rcvd=0;
    int bytes_sent=0, bytes_rcvd=0;
	cerr << "about to rcv\n";
	Timer total_time, loop_time;
	double send_time=0;
	total_time.start();
	data=new char[mtu];
	bytes_rcvd=1;	

	try
	{
		out_sock.connect(remote_ip.c_str(), remote_port);
		cerr << "Server connected to client\n";

   		accept_sock.bind(local_ip, local_port);
   		accept_sock.listen();
		int s = accept_sock.accept();
		if (s==-1)
			cerr << "Invalid accept\n";
   		in_sock.set_sockd(s);
   		cerr << "Server received connection from incoming Mark5\n";
	}
	catch(CUDTException e)
	{
		cerr << "error msg: " << e.getErrorMessage() << endl;
	}
	int loops=0;
	int mtus_per_20ms=double(rate)*20e-3/(8.0*double(mtu));
	cerr << "mtus_per_20ms==" << mtus_per_20ms << endl;
   	do {
		loop_time.start();
		if (loops%mtus_per_20ms==0)	
			usleep_that_works(10000);
		// cerr << "Elapsed: " << loop_time.elapsed() << endl;
       	bytes_rcvd=in_sock.recv(data, sock_mtu);
		if (bytes_rcvd<=0)
			break;
		// cerr << "rcvd: " << bytes_rcvd << endl;
		// cerr << "tbr:  " << total_bytes_rcvd << endl;
		total_bytes_rcvd+=bytes_rcvd;
		bytes_sent=out_sock.send(data, bytes_rcvd);
		if (bytes_sent!=bytes_rcvd)
			cerr << "Short send\n";
		total_bytes_sent+=bytes_sent;
		loop_time.stop();
		send_time+=loop_time.elapsed();
		// double transmit_rate = (double)bytes_sent/loop_time.elapsed();
		// cerr << "bytes_sent:         " << bytes_sent << endl;
        // cerr << "bytes_rcvd:         " << bytes_rcvd << endl;
		// cerr << "elapsed(s):          " << loop_time.elapsed() << endl;
		// cerr << "transmit_rate(bps): " << transmit_rate << endl;
		loops++;
	} while (bytes_rcvd>0);
	total_time.stop();
	usleep(1000);
		cerr << "total_bytes_sent:    " << total_bytes_sent << endl;
        cerr << "total_bytes_rcvd:    " << total_bytes_rcvd << endl;
	cerr << "total_time(s):       " << total_time.elapsed() << endl;	
	cerr << "transmit_rate:  (bps)" 
		<< 8.0*(double)total_bytes_sent/total_time.elapsed()
		<< endl;
	cerr << "transmit_rate:  (bps)" 
		<< 8.0*(double)total_bytes_sent/send_time
		<< endl;
	out_sock.close();
	in_sock.shutdown(SHUT_RDWR);
	in_sock.close();
	accept_sock.shutdown(SHUT_RDWR);
	accept_sock.close();
}

void udp_sock2(const Options& o)
{
   	TCPSocket accept_sock, in_sock;
	UDPSocket out_sock;

   	string local_ip=string(o.get_local_ip());
   	string remote_ip=string(o.get_remote_ip());
   	int local_port(o.get_local_port());
	int remote_port(o.get_remote_port());
	const int mtu(o.get_mtu());
	const int mem_thresh(o.get_mem_thresh());
	const int sock_mtu(o.get_sock_mtu());
	const int rate(o.get_rate());
	
	cerr << "udp_sock2" << endl;
	cerr << "Remote IP:   " << remote_ip << endl;
	cerr << "Local IP:    " << local_ip << endl;
	cerr << "Remote port: " << remote_port << endl;
	cerr << "Local port:  " << local_port << endl;
	cerr << "mtu:         " << mtu << endl;
	cerr << "sock_mtu:    " << sock_mtu << endl;

    int total_bytes_sent=0, total_bytes_rcvd=0;
    int bytes_sent=0, bytes_rcvd=0;
	cerr << "about to rcv\n";
	Timer total_time, loop_time;
	double send_time=0;
	total_time.start();
	in_data=new char[sock_mtu];
	out_data=new char[mtu+2];
	write_ptr=NULL;
	bytes_rcvd=1;	
	unsigned int seq_no=0;

	try
	{
		out_sock.connect(remote_ip.c_str(), remote_port);
		cerr << "Server connected to client\n";

   		accept_sock.bind(local_ip, local_port);
   		accept_sock.listen();
		int s = accept_sock.accept();
		if (s==-1)
			cerr << "Invalid accept\n";
   		in_sock.set_sockd(s);
   		cerr << "Server received connection from incoming Mark5\n";
	}
	catch(CUDTException e)
	{
		cerr << "error msg: " << e.getErrorMessage() << endl;
	}
	int loops=0;
	int mtus_per_20ms=double(rate)*20e-3/(8.0*double(mtu));
	cerr << "mtus_per_20ms==" << mtus_per_20ms << endl;
   	do {
		loop_time.start();
		if (loops%mtus_per_20ms==0)	
			usleep_that_works(10000);
		// cerr << "Elapsed: " << loop_time.elapsed() << endl;
       	bytes_rcvd=in_sock.recv(in_data, sock_mtu);
		if (bytes_rcvd<=0) {
			loop_time.stop();
			++loops;
			cerr << "0 read\n";
			continue;
		}
		// cerr << "rcvd: " << bytes_rcvd << endl;
		// cerr << "tbr:  " << total_bytes_rcvd << endl;
		total_bytes_rcvd+=bytes_rcvd;
		write_ptr=out_data;
		out_data[0]=(seq_no>>8)&0xff;
		out_data[1]=seq_no&0xff;
		write_ptr+=2;
		// cerr << "bytes received: " << bytes_rcvd << endl;
		memcpy(write_ptr, in_data, bytes_rcvd);
		do {
			bytes_sent=out_sock.send(out_data, bytes_rcvd+2);
		} while (bytes_sent<=0);
		// cerr << "bytes sent: " << bytes_sent << endl;
		if (bytes_sent!=(bytes_rcvd+2))
			cerr << "Short send\n";
		++seq_no;
		total_bytes_sent+=bytes_sent;
		loop_time.stop();
		send_time+=loop_time.elapsed();
		// double transmit_rate = (double)bytes_sent/loop_time.elapsed();
		// cerr << "bytes_sent:         " << bytes_sent << endl;
        // cerr << "bytes_rcvd:         " << bytes_rcvd << endl;
		// cerr << "elapsed(s):          " << loop_time.elapsed() << endl;
		// cerr << "transmit_rate(bps): " << transmit_rate << endl;
		++loops;
	} while (bytes_rcvd>0);
	total_time.stop();
	usleep(1000);
		cerr << "total_bytes_sent:    " << total_bytes_sent << endl;
        cerr << "total_bytes_rcvd:    " << total_bytes_rcvd << endl;
	cerr << "total_time(s):       " << total_time.elapsed() << endl;	
	cerr << "transmit_rate:  (bps)" 
		<< 8.0*(double)total_bytes_sent/total_time.elapsed()
		<< endl;
	cerr << "transmit_rate:  (bps)" 
		<< 8.0*(double)total_bytes_sent/send_time
		<< endl;
	out_sock.close();
	in_sock.shutdown(SHUT_RDWR);
	in_sock.close();
	accept_sock.shutdown(SHUT_RDWR);
	accept_sock.close();
}

u_int64_t get_usec_since(struct timeval *old_time)
{
    struct timeval now;
    u_int64_t      result = 0;

    /* get the current time */
    gettimeofday(&now, NULL);

    /* return the elapsed time */
    while (now.tv_sec > old_time->tv_sec) {
    result += 1000000;
    --now.tv_sec;
    }
    return result + (now.tv_usec - old_time->tv_usec);

    /*------------------------------------------------------------
     * We used to calculate it like this, but the above is
     * usually a bit faster in the general case.  Note that we
     * have BIG problems if old_time is in the future, however...
     *------------------------------------------------------------*/
    /* return 1000000LL * (now.tv_sec - old_time->tv_sec) + now.tv_usec - old_time->tv_usec; */
}

void usleep_that_works(u_int64_t usec)
{
    u_int64_t      sleep_time = (usec / 10000) * 10000;  /* the amount of time to sleep */
    struct timeval delay, now;

    /* get the current time */
    gettimeofday(&now, NULL);

    /* do the basic sleep */
    delay.tv_sec  = sleep_time / 1000000;
    delay.tv_usec = sleep_time % 1000000;
    select(0, NULL, NULL, NULL, &delay);

    /* and spin for the rest of the time */
    while (get_usec_since(&now) < usec);
}
