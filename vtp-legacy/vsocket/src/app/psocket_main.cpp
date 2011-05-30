#include <PSocket.h>
#include <Options.h>
#include <Utils.h>
#include <File.h>
#include <stdio.h>
#include <deque>
#include <unistd.h>

void client(const Options& o);
void server(const Options& o);
void testmode(const Options& o);

int main(int argc, char** argv)
{
	Options o(argc, argv);
	if (o.get_debug_level()>0) {
		Logsystem::info_logger()->enable_logging();
		Logsystem::warning_logger()->enable_logging();
		Logsystem::debug_logger()->enable_logging();
	}
	Logsystem::error_logger()->enable_logging();
	try {
		switch (o.get_mode()) {
		case CLIENT:
			client(o);
			break;
		case SERVER:
			server(o);
			break;
		case TEST:
			testmode(o);
			break;
		}
	} catch (exception e) {
		(*Logsystem::debug_logger()) << "exception: " << e.what() << "\n";
	}
	return(0);
}

void client(const Options& o)
{
	Logger& debug_logger=(*Logsystem::debug_logger());
	Logger& error_logger=(*Logsystem::error_logger());
	debug_logger << "client()\n";
	debug_logger << "    max_buf==" << o.get_max_buf() << "\n";
	debug_logger << "    remote_ip==" << o.get_remote_ip() << "\n";
	debug_logger << "    remote_port==" << o.get_remote_port() << "\n";
	debug_logger << "    streams==" << o.get_streams() << "\n";
	debug_logger << "    from_file==" << o.get_from_file() << "\n";
	debug_logger << "    host_mtu==" << o.get_host_mtu() << "\n";
	debug_logger << "    network_mtu==" << o.get_network_mtu() << "\n";
	debug_logger << "    rtt==" << o.get_rtt() << "\n";
	debug_logger << "    path_bandwidth==" << o.get_path_bandwidth() << "\n";

	// Constants.
	const int host_mtu=o.get_host_mtu();
	const int network_mtu=o.get_network_mtu();
	// File.
	File in(o.get_from_file().c_str(), File::in);
	// in.set_blocking(File::nonblocking);
	int bytes_sent=0;
	// Buffers.
	deque<SocketBufferCtrlBlk> send_bufs;
	SocketBufferCtrlBlk s(network_mtu);
	int total_bytes_sent=0;
	int sock_buf_size=int(2.0*double(o.get_rtt())*1e-3*
						double(o.get_path_bandwidth())/8.0);
	// Socket creation and connection.
	// PSocket p(o.get_streams(), network_mtu);
	TCPSocket p(network_mtu);
	p.set_so_sndbuf(sock_buf_size);
	p.connect(o.get_remote_ip(), o.get_remote_port());
	p.set_blocking(Socket::nonblocking);
	int bytes_read=0;

	// Send loop.
	do {
		bytes_read=in.read((char*)&(s._sb[0]), network_mtu);	
		if (bytes_read>0) {
			s._bytes_read=bytes_read;
			s._bytes_sent=0;
			send_bufs.push_back(s);	
		}
		if (bytes_read==-1)  {
			perror("in.read(): ");
		}
		if (send_bufs.empty())  {
			if (bytes_read==0)
				break;
			continue;
		}
		// cerr << send_bufs.size() << endl;
		SocketBufferCtrlBlk& t = *send_bufs.begin();
		bytes_sent=p.send(t._sb, t._bytes_sent, t._bytes_read-t._bytes_sent);
		if (bytes_sent==-1)
			continue;
		//if (bytes_sent==-1)
			//perror("p.send(): ");
		if (bytes_sent>0) 
			t._bytes_sent+=bytes_sent;
		if ((t._bytes_read-t._bytes_sent)<=0)
			send_bufs.pop_front();
		if (bytes_sent==0) {
			cerr << "    bytes_sent: " << bytes_sent << endl;
			break;
		}
		debug_logger << "    bytes_sent: " << bytes_sent << "\n";
		// if (bytes_sent<network_mtu)
			// cerr << " short send\n";
		total_bytes_sent+=bytes_sent;
	} while (true);
	// Close file.
	in.close();
	// Close down the sending half of this connection.
	p.shutdown(SHUT_WR);
	// Application layer ACK.
	int bytes_rcvd=p.recv(s._sb, 1);
	cout << "Final read: " << bytes_rcvd << endl;
	// Close down the PSocket.
	p.close();
	debug_logger << "-client()\n";
}

void server(const Options& o)
{
	Logger& debug_logger=(*Logsystem::debug_logger());
	Logger& error_logger=(*Logsystem::error_logger());
	debug_logger << "server()\n";
	debug_logger << "    max_buf==" << o.get_max_buf() << "\n";
	debug_logger << "    local_ip==" << o.get_local_ip() << "\n";
	debug_logger << "    local_port==" << o.get_local_port() << "\n";
	debug_logger << "    streams==" << o.get_streams() << "\n";
	debug_logger << "    to_file==" << o.get_to_file() << "\n";
	debug_logger << "    host_mtu==" << o.get_host_mtu() << "\n";
	debug_logger << "    network_mtu==" << o.get_network_mtu() << "\n";
	debug_logger << "    rtt==" << o.get_rtt() << "\n";
	debug_logger << "    path_bandwidth==" << o.get_path_bandwidth() << "\n";

	// Constants.
	const int max_buf=o.get_max_buf();
	const int host_mtu=o.get_host_mtu();
	const int network_mtu=o.get_network_mtu();
	// File.
	File out(o.get_to_file().c_str(), File::out);
	out.set_blocking(File::nonblocking);
	// Buffers.
	SocketBufferCtrlBlk s(network_mtu);
	deque<SocketBufferCtrlBlk> rcv_bufs;
	int bytes_rcvd=0;
	int recv_loops=host_mtu/network_mtu;
	int sock_buf_size=int(2.0*double(o.get_rtt())*1e-3*
						double(o.get_path_bandwidth())/8.0);

	// PSocket p(o.get_streams(), network_mtu);
	// p.set_so_rcvbuf(sock_buf_size);
	// p.bind(o.get_local_ip(), o.get_local_port());
	// p.listen();
	// p.accept();
	// p.set_blocking(Socket::nonblocking);
	// Sockets.
	TCPSocket t(network_mtu);
	t.set_so_sndbuf(sock_buf_size);
	t.bind(o.get_local_ip(), o.get_local_port());
	t.listen();
	int sd=t.accept();
	TCPSocket p(sd, network_mtu);
	p.set_blocking(Socket::nonblocking);
	int bytes_read=0;

	Timer total, loop;
	total.start();
	int total_bytes_rcvd=0;
	bool rcv=true;
	while  (rcv) {
        do {
			// We must recv() bytes_left bytes from this socket.
			// The PSocket will not increment until we receive the
			// amount of data we request. The only other way
			// out of this loop is if the connection is shutdown()
			// from the client side, in which case bytes_rcvd will
			// be -1.
           	bytes_rcvd=p.recv(s._sb, network_mtu);
			if (bytes_rcvd>0) {
				s._bytes_read=bytes_rcvd;
				s._bytes_sent=0;
				rcv_bufs.push_back(s);
			}
            if (bytes_rcvd==0) {
				rcv=false;
            }
            // if (bytes_rcvd==-1)
				// perror("p.recv(): ");
            // if ((bytes_rcvd<network_mtu)&&bytes_rcvd!=-1)
                // cout << " short recv:" << bytes_rcvd << endl;
            // cout << "bytes: " << total_bytes_rcvd << endl;
            total_bytes_rcvd+=bytes_rcvd;
			if (rcv_bufs.empty()) {
				if (bytes_rcvd==0)
					break;
				continue;
			}
			// cerr << rcv_bufs.size() << endl;
        	SocketBufferCtrlBlk& t = *rcv_bufs.begin();
			int bytes_wrtn=0;
			bytes_wrtn=out.write((char*)&(t._sb[0]), t._bytes_read-t._bytes_sent);
            if (bytes_wrtn==-1)
				perror("out.write(): ");
			if (bytes_wrtn>0)
				t._bytes_sent+=bytes_wrtn;
			if ((t._bytes_read-t._bytes_sent)>0) {
				error_logger << "Incomplete file write\n";
				exit(1);
			} else
				rcv_bufs.pop_front();
        } while (true);
		// loop.stop();
	}
    int ack_bytes=p.send(s._sb, 1);
    cout << "Ack_bytes: " << ack_bytes << endl;
	total.stop();
	cout << "Total time(s)==" << total.elapsed() << "\n";
	cout << "Total bytes==" << total_bytes_rcvd << "\n";
	cout << "Average rate(Bps)==" << total_bytes_rcvd/total.elapsed() 
				<< "\n";
	cout << "Average rate(bps)==" << 8.0*total_bytes_rcvd/total.elapsed() 
				<< "\n";

	out.close();
	p.shutdown(SHUT_RDWR);
	p.close();
	debug_logger << "-server()\n";
}

void testmode(const Options& o)
{
	Logger& debug_logger=(*Logsystem::debug_logger());
	Logger& error_logger=(*Logsystem::error_logger());
	const int test=o.get_test();
	const int network_mtu=o.get_network_mtu();
	debug_logger << "testmode()\n";
	debug_logger << "    to_file==" << o.get_to_file() << "\n";
	debug_logger << "    test==" << o.get_test() << "\n";
	debug_logger << "    network_mtu==" << o.get_network_mtu() << "\n";

	deque<SocketBufferCtrlBlk> send_bufs;
	SocketBufferCtrlBlk ssb(1024);
	SocketBuffer s(1024);
	cout << "ssb.size() " << ssb._sb.size() << endl;
	cout << "s.size() " << s.size() << endl;

	Timer total, loop;
	total.start();
	const int iterations=100000;
	for (int i=0; i<iterations; i++) {
		send_bufs.push_back(ssb);
	}
	total.stop();
	cout << "Test pushback()" << endl;
	cout << "Iterations   " << iterations << endl;
	cout << "Elapsed      " << total.elapsed() << endl;
	cout << "Timeper      " << total.elapsed()/iterations << endl;

	total.start();
	for (int i=0; i<iterations; i++) {
		send_bufs.pop_front();
	}
	total.stop();
	cout << "Test pop_front()" << endl;
	cout << "Iterations   " << iterations << endl;
	cout << "Elapsed      " << total.elapsed() << endl;
	cout << "Timeper      " << total.elapsed()/iterations << endl;

	total.start();
	for (int i=0; i<iterations; i++) {
		ssb._sb=s;
	}
	total.stop();
	cout << "Test StringBuffer operator=()" << endl;
	cout << "Iterations   " << iterations << endl;
	cout << "Elapsed      " << total.elapsed() << endl;
	cout << "Timeper      " << total.elapsed()/iterations << endl;


	string name2("test.dat"), name1("input.dat");
	File in(name1, File::in);
	File out(name2, File::out);

	int bytes_read;
	deque<SocketBufferCtrlBlk> rcv_bufs;
	do {
		SocketBuffer& t = ssb._sb;
		bytes_read=in.read((char*)&(t[0]), network_mtu);
		if (bytes_read==-1)
			continue;
		ssb._bytes_read=bytes_read;
		rcv_bufs.push_back(ssb);
		cout << "rcv_bufs.size() " << rcv_bufs.size() << endl;
	} while (bytes_read>0);	
	cout << "Completed loading\n";

	do {
		SocketBufferCtrlBlk& u = *rcv_bufs.begin();
		SocketBuffer& v = u._sb;
		int bytes_wrtn=out.write((char*)&(v[0]), u._bytes_read);
		if (bytes_wrtn==-1)
			continue;
		if (bytes_wrtn!=u._bytes_read)
			error_logger << "incomplete file write\n";
		else
			rcv_bufs.pop_front();
		cout << "rcv_bufs.size() " << rcv_bufs.size() << endl;
	} while (!rcv_bufs.empty());	

	in.close();
	out.close();



	// in.set_blocking(File::nonblocking);
#if 0
	../vsocket.app -t 1
	ssb.size() 1024
	s.size() 1024
	Test pushback()
	Iterations   100000
	Elapsed      0.65664
	Timeper      6.5664e-06
	Test pop_front()
	Iterations   100000
	Elapsed      0.114567
	Timeper      1.14567e-06
	Test StringBuffer operator=()
	Iterations   100000
	Elapsed      0.037959
	Timeper      3.7959e-07
#endif

	// out.fill('0');
	// out.setf(ios::right);
	// for (int i=0; i<1024; i++) {
		// out.width(10);
		// out << i << ":";
		// for (int j=0; j<69; j++)
			// out << j%10;
		// out << endl;
	// }
	// out.close();
	(*Logsystem::debug_logger()) << "-testmode()\n";
}


