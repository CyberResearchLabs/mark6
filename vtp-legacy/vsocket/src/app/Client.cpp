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

#include <Client.h>

void Client::Client()
{
}

void Client::run(const Options& o)
{
	DEBUG_FUNC("Client") << "client()\n";
	DEBUG_FUNC("Client") << "    max_buf==" << o.get_max_buf() << "\n";
	DEBUG_FUNC("Client") << "    remote_ip==" << o.get_remote_ip() << "\n";
	DEBUG_FUNC("Client") << "    remote_port==" << o.get_remote_port() << "\n";
	DEBUG_FUNC("Client") << "    streams==" << o.get_streams() << "\n";
	DEBUG_FUNC("Client") << "    from_file==" << o.get_from_file() << "\n";
	DEBUG_FUNC("Client") << "    host_mtu==" << o.get_host_mtu() << "\n";
	DEBUG_FUNC("Client") << "    network_mtu==" << o.get_network_mtu() << "\n";
	DEBUG_FUNC("Client") << "    rtt==" << o.get_rtt() << "\n";
	DEBUG_FUNC("Client") << "    path_bandwidth==" << o.get_path_bandwidth() << "\n";

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
	p.connect(o.get_remote_ip(), o.get_remote_port());
	// p.thread_create();
	// p.set_blocking(Socket::nonblocking);
	int bytes_read=0;

	// Send loop.
	do {
		bytes_read=in.read((char*)&(s[0]), network_mtu);	
		if (bytes_read<=0) {
			continue;
		}
        int bytes_sent=p.send(s, bytes_read);
		usleep(10000);
		if (bytes_sent>0) 
			total_bytes_sent+=bytes_sent;
		if (bytes_sent<network_mtu)
			cerr << " short send\n";
		// DEBUG_FUNC("Client") << "    bytes_sent: " << bytes_sent << "\n";
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
	DEBUG_FUNC("Client") << "-client()\n";
}

