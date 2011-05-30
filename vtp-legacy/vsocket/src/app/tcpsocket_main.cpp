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
	SocketBuffer s;
	s.resize(network_mtu);
	int total_bytes_sent=0;
	int sock_buf_size=int(2.0*double(o.get_rtt())*1e-3*
						double(o.get_path_bandwidth())/8.0);
	TCPSocket p(network_mtu);
	p.set_so_sndbuf(sock_buf_size);
	p.connect(o.get_remote_ip(), o.get_remote_port());
	int bytes_read=0;
	// Send loop.
	do {
		bytes_read=in.read((char*)&(s[0]), network_mtu);	
		if (bytes_read==-1)  {
			perror("in.read(): ");
			continue;
		}
		bytes_sent=p.send(s, bytes_read);
		if (bytes_sent==-1)
			continue;
		if (bytes_sent==0) {
			cerr << "    bytes_sent: " << bytes_sent << endl;
			break;
		}
		debug_logger << "    bytes_sent: " << bytes_sent << "\n";
		if (bytes_sent<network_mtu)
			cerr << " short send\n";
		total_bytes_sent+=bytes_sent;
	} while (true);
	// Close file.
	in.close();
	// Close down the sending half of this connection.
	p.shutdown(SHUT_WR);
	// Application layer ACK.
	int bytes_rcvd=p.recv(s, 1);
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
	debug_logger << "    local_ip==" << o.get_local_ip() << "\n";
	debug_logger << "    local_port==" << o.get_local_port() << "\n";
	debug_logger << "    streams==" << o.get_streams() << "\n";
	debug_logger << "    to_file==" << o.get_to_file() << "\n";
	debug_logger << "    host_mtu==" << o.get_host_mtu() << "\n";
	debug_logger << "    network_mtu==" << o.get_network_mtu() << "\n";
	debug_logger << "    rtt==" << o.get_rtt() << "\n";
	debug_logger << "    path_bandwidth==" << o.get_path_bandwidth() << "\n";

	// Constants.
	const int host_mtu=o.get_host_mtu();
	const int network_mtu=o.get_network_mtu();
	// File.
	File out(o.get_to_file().c_str(), File::out);
	// Buffers.
	SocketBuffer s;
	s.resize(network_mtu);
	int bytes_rcvd=0;
	int recv_loops=host_mtu/network_mtu;
	int sock_buf_size=int(2.0*double(o.get_rtt())*1e-3*
						double(o.get_path_bandwidth())/8.0);

	TCPSocket t(network_mtu);
	t.set_so_sndbuf(sock_buf_size);
	t.bind(o.get_local_ip(), o.get_local_port());
	t.listen();
	int sd=t.accept();
	TCPSocket p(sd, network_mtu);
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
           	bytes_rcvd=p.recv(s, network_mtu);
            if (bytes_rcvd==0) {
				rcv=false;
            }
            // if (bytes_rcvd==-1)
				// perror("p.recv(): ");
            // if ((bytes_rcvd<network_mtu)&&bytes_rcvd!=-1)
                // cout << " short recv:" << bytes_rcvd << endl;
            // cout << "bytes: " << total_bytes_rcvd << endl;
            total_bytes_rcvd+=bytes_rcvd;
			int bytes_wrtn=0;
			bytes_wrtn=out.write((char*)&(s[0]), bytes_rcvd);
            if (bytes_wrtn==-1)
				perror("out.write(): ");
        } while (true);
		// loop.stop();
	}
    int ack_bytes=p.send(s, 1);
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

	(*Logsystem::debug_logger()) << "-testmode()\n";
}


