/* 
 * This software is released under the terms of the MIT License(included below). *  
 * Copyright (c) 2003 MIT Haystack Observatory 
 *  
 * Permission is hereby granted, free of charge, to any person obtaining a   
 * copy of this software and associated documentation files (the "Software"),    * to deal in the Software without restriction, including without limitation   
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   
 * and/or sell copies of the Software, and to permit persons to whom the  
 * Software is furnished to do so, subject to the following conditions: 
 *  
 * The above copyright notice and this permission notice shall be included in    * all copies or substantial portions of the Software. 
 *   
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE  * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER   
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE * SOFTWARE. 
 * 
 */

#include <Server.h>

void Server::Server()
{
	START_FUNC("Server")
	END_FUNC("Server")
}

void Server::Server(const Options& o)
{
	START_FUNC("Server")
	DEBUG_LOGGER("Server") << "    max_buf==" << o.get_max_buf() << "\n";
	DEBUG_LOGGER("Server") << "    local_ip==" << o.get_local_ip() << "\n";
	DEBUG_LOGGER("Server") << "    local_port==" << o.get_local_port() << "\n";
	DEBUG_LOGGER("Server") << "    streams==" << o.get_streams() << "\n";
	DEBUG_LOGGER("Server") << "    to_file==" << o.get_to_file() << "\n";
	DEBUG_LOGGER("Server") << "    host_mtu==" << o.get_host_mtu() << "\n";
	DEBUG_LOGGER("Server") << "    network_mtu==" << o.get_network_mtu() << "\n";
	DEBUG_LOGGER("Server") << "    rtt==" << o.get_rtt() << "\n";
	DEBUG_LOGGER("Server") << "    path_bandwidth==" << o.get_path_bandwidth() << "\n";

	// Constants.
	const int max_buf=o.get_max_buf();
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
	p.bind(o.get_local_ip(), o.get_local_port());
	// p.thread_create();
	// p.set_blocking(Socket::nonblocking);

	Timer total, loop;
	total.start();
	int total_bytes_rcvd=0;
	int total_bytes_wrtn=0;
	bool rcv=true;
	int timeout=10;
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
				DEBUG_LOGGER("Server") << " main recv(): 0 bytes\n";
				usleep(1000000);
				--timeout;
				if (timeout)
					continue;
				else {
					rcv=false;
					break;
				}
            }
            if (bytes_rcvd==-1) {
				perror("p.recv(): ");
				continue;
			}
			cerr << " bytes received: " << bytes_rcvd << "\n";
            total_bytes_rcvd+=bytes_rcvd;
			int bytes_wrtn=out.write((char*)&(s[0]), bytes_rcvd);
            if (bytes_wrtn==-1)
				perror("out.write(): ");
			if (bytes_wrtn>0) {
				total_bytes_wrtn+=bytes_wrtn;
				cerr << " bytes written: " << total_bytes_wrtn << "\n";
			}
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
	START_FUNC("Server")
}

