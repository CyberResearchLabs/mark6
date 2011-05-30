#include <common.h>
#include <Logsystem.h>
#include <TCPSocket.h>
#include <Options.h>
#include <fstream>
#include <vector>

int
main(int argc, char** argv)
{
	Options o(argc, argv);
	string log_name(o.get_log_file());
	ofstream log(log_name.c_str(), ios::out | ios::trunc);
	const int network_mtu=5;
	char buf[network_mtu+1];
	buf[network_mtu+1]='\0';
	TCPSocket sock(network_mtu);
	sock.set_tcp_nodelay(1);
	sock.connect(o.get_remote_ip(), o.get_remote_port());
	while (!feof(stdin)) {
		// delimiter extracted but not stored. NULL added to 
		// end of character array. Maximum string size read is
		// network_mtu-1 (one place must be reserved for appended
		// NULL.
		SocketBuffer sbuf(network_mtu+1);
		int bytes_read=fread((void*)&(sbuf[0]), 1, network_mtu, stdin);
        sbuf[network_mtu+1]='\0';
		printf("bytes_read %d\n", bytes_read);
		int bytes_sent=sock.send(sbuf, bytes_read);
		printf("bytes_sent %d\n", bytes_sent);
	}
	sock.close();
}
