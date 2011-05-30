#include <VRSocket.h>
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
	switch (o.get_debug_level()) {
	case 3:
		Logsystem::debug_logger()->enable_logging();
	case 2:
		Logsystem::info_logger()->enable_logging();
	case 1:
		Logsystem::warning_logger()->enable_logging();
	case 0:
		Logsystem::error_logger()->enable_logging();
		// string info("info.txt");
		// string warn("warn.txt");
		// string deb("deb.txt");
		// Logsystem::info_logger()->set_stream(info);
		// Logsystem::warning_logger()->set_stream(warn);
		// Logsystem::debug_logger()->set_stream(deb);
	default:
		break;
	}
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
	cout << "client()\n";
	cout << "    remote_ip==" << o.get_remote_ip() << "\n";
	cout << "    remote_port==" << o.get_remote_port() << "\n";
	cout << "    streams==" << o.get_streams() << "\n";
	cout << "    from_file==" << o.get_from_file() << "\n";
	cout << "    host_mtu==" << o.get_host_mtu() << "\n";
	cout << "    network_mtu==" << o.get_network_mtu() << "\n";
	cout << "    rtt==" << o.get_rtt() << "\n";
	cout << "    path_bandwidth==" << o.get_path_bandwidth() << "\n";
	cout << "    tvg==" << o.get_tvg() << "\n";
	cout << "    tvg_sz==" << o.get_tvg_sz() << "\n";

	// Constants.
	const int host_mtu=o.get_host_mtu();
	const int network_mtu=o.get_network_mtu();
	// File.
	File in(o.get_from_file().c_str(), File::in);
	int bytes_sent=0;
	// Buffers.
	SocketBuffer s(network_mtu);
	int total_bytes_sent=0;
	int sock_buf_size=int(2.0*double(o.get_rtt())*1e-3*
						double(o.get_path_bandwidth())/8.0);
	// Socket creation and connection.
	VRSocket p(VRSocket::sender, network_mtu);
	p.connect(o.get_remote_ip(), o.get_remote_port(), o.get_remote_port()+1);
	int bytes_read=0;

	// Send loop.
	do {
		bytes_read=in.read((char*)&(s[0]), network_mtu);	
		if (bytes_read<=0) {
			if (p.get_send_buf_size()==0 && p.get_outstanding_buf_size()==0) {
				p.join();
				break;
			}
			continue;
		}
		// cerr << "Bytes read: " << bytes_read << endl;	
        bytes_sent=p.send(s, bytes_read);
		if (bytes_sent>0) 
			total_bytes_sent+=bytes_sent;
		if (bytes_sent<network_mtu)
			debug_logger << " short send\n";
		if (bytes_sent==-1)
			break;
	} while (true);
	// Close file.
	in.close();
	// Close down the sending half of this connection.
	// p.shutdown(SHUT_WR);
	cout << "    total_bytes_sent: " << total_bytes_sent << "\n";
	// Application layer ACK.
	// int bytes_rcvd=p.recv(s, 1);
	// cout << "Final read: " << bytes_rcvd << endl;
	// Close down the PSocket.
	p.close();
	cout << "-client()\n";
}

void server(const Options& o)
{
	Logger& debug_logger=(*Logsystem::debug_logger());
	cout << "server()\n";
	cout << "    local_ip==" << o.get_local_ip() << "\n";
	cout << "    local_port==" << o.get_local_port() << "\n";
	cout << "    streams==" << o.get_streams() << "\n";
	cout << "    to_file==" << o.get_to_file() << "\n";
	cout << "    host_mtu==" << o.get_host_mtu() << "\n";
	cout << "    network_mtu==" << o.get_network_mtu() << "\n";
	cout << "    rtt==" << o.get_rtt() << "\n";
	cout << "    path_bandwidth==" << o.get_path_bandwidth() << "\n";

	// Constants.
	const int host_mtu=o.get_host_mtu();
	const int network_mtu=o.get_network_mtu();
	// File.
	File out(o.get_to_file().c_str(), File::out);
	// Buffers.
	SocketBuffer s(network_mtu);
	int bytes_rcvd=0;
	int recv_loops=host_mtu/network_mtu;
	int sock_buf_size=int(2.0*double(o.get_rtt())*1e-3*
						double(o.get_path_bandwidth())/8.0);
	VRSocket p(VRSocket::receiver, network_mtu);
	p.bind(o.get_local_ip(), o.get_local_port(), o.get_local_port()+1);
	p.listen();
	p.accept();

	Timer total, loop;
	total.start();
	int total_bytes_rcvd=0;
	int total_bytes_wrtn=0;
	bool rcv=true;
	while  (rcv) {
        do {
           	bytes_rcvd=p.recv(s, network_mtu);
            if (bytes_rcvd==0)
				debug_logger << " main recv(): 0 bytes\n";
            if (bytes_rcvd==-1) {
				rcv=false;
				break;
			}
			// cerr << " bytes received: " << bytes_rcvd << "\n";
            total_bytes_rcvd+=bytes_rcvd;
			int bytes_wrtn=out.write((char*)&(s[0]), bytes_rcvd);
            if (bytes_wrtn==-1)
				perror("out.write(): ");
			if (bytes_wrtn>0) {
				total_bytes_wrtn+=bytes_wrtn;
			}
        } while (true);
		// loop.stop();
	}
    int ack_bytes=p.send(s, 1);
	cout << " bytes written: " << total_bytes_wrtn << "\n";
    cout << "Ack_bytes: " << ack_bytes << endl;
	total.stop();
	cout << "Total time(s)==" << total.elapsed() << "\n";
	cout << "Total bytes==" << total_bytes_rcvd << "\n";
	cout << "Average rate(Bps)==" << total_bytes_rcvd/total.elapsed() 
				<< "\n";
	cout << "Average rate(bps)==" << 8.0*total_bytes_rcvd/total.elapsed() 
				<< "\n";

	out.close();
	// p.shutdown(SHUT_RDWR);
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



	// in.set_blocking(File::NONBLOCKING);
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


