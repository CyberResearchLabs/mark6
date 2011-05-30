#include <PSocket.h>
#include <Options.h>
#include <Utils.h>
#include <File.h>
#include <stdio.h>
#include <deque>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include <netdb.h>
#include <syslog.h>


void client(const Options& o);
void server(const Options& o);

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
	}
	try {
		switch (o.get_mode()) {
		case CLIENT:
			client(o);
			break;
		case SERVER:
			server(o);
			break;
		}
	} catch (exception e) {
		(*Logsystem::debug_logger()) << "exception: " << e.what() << "\n";
	}
	return(0);
}

void server(const Options& o)
{
	Logger& debug_logger=(*Logsystem::debug_logger());
	Logger& error_logger=(*Logsystem::error_logger());
	debug_logger << "server()\n";
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
	// Buffers.
	SocketBuffer s;
	s.resize(network_mtu);
	TCPSocket ctrl_sock(network_mtu);
	string file_name=o.get_to_file();
	assert( strncpy((char*)&s[2], file_name.c_str(), (int)file_name.size()) );	
	s[0]='a';
	s[1]=file_name.size();
	debug_logger << "file_name:        " << file_name << "\n";
	debug_logger << "file_name:        " << (char*)&s[2] << "\n";
	debug_logger << "file_name.size(): " << (int)file_name.size() << "\n";
	debug_logger << "s.size(): " << (int)s.size() << "\n";

	// 1. Connect to remote egaedrive port.
	struct servent *service;
	service=getservbyname("egaedrive", "tcp");
	int egaedrive_port=service->s_port;
	service=getservbyname("egaedata", "tcp");
	int egaedata_port=service->s_port;
	service=getservbyname("m5drive", "tcp");
	int m5drive_port=service->s_port;
	service=getservbyname("m5data", "tcp");
	int m5data_port=service->s_port;

	debug_logger << "egaedrive: " << ntohs(egaedrive_port) << ENDL;
	debug_logger << "egaedata:  " << ntohs(egaedata_port) << ENDL;
	debug_logger << "m5drive:   " << ntohs(m5drive_port) << ENDL;
	debug_logger << "m5data:    " << ntohs(m5data_port) << ENDL;
	
	TCPSocket egae_drive(network_mtu);
	egae_drive.connect(o.get_remote_ip(), egaedrive_port);
	egae_drive.send(s, network_mtu);

	// 6. Wait for response from remote egaedrive
	s.clear();
	int bytes_read=egae_drive.recv(s, network_mtu);
	char cmd=s[0];
	char length=s[1];
	int i=0;
	string response;
	for (i=2; i<2+length-'0'; ++i)
		response+=s[i];
	response+='\0';
	debug_logger << "5. Response from egaedrive.\n";
	debug_logger << "bytes_read: " << bytes_read << ENDL;
	debug_logger << "Command:   " << cmd << ENDL;
	debug_logger << "Length:    " << length << ENDL;
	debug_logger << "Response:    " << response << ENDL;

	// 7. Establish data connection to remote egaedata port.
	TCPSocket egae_data(network_mtu);
	egae_data.connect(o.get_remote_ip(), egaedata_port);

	// 8.1 Bind to local m5data port.
	TCPSocket m5_datab(network_mtu);
	m5_datab.bind(o.get_local_ip(), m5data_port);

	// 8.2 Prepare to Connect to local m5drive port and setup M5.
	TCPSocket m5_drive(network_mtu);
	m5_drive.connect("127.0.0.1", m5drive_port);

	// Need to fork() to be able to send command and receive
	// data at the same time.
	pid_t child_pid=fork();
	if (child_pid==0) {
		// 7.3.1 to Connect to local m5drive port and setup M5.
		// I am the child. Wait for parent to setup before sending command.
		usleep(5000000);
		int l=snprintf((char*)&s[0], network_mtu, 
					"disc2net=connect:localhost:%d:%d;\n",
					0, 10000);
		m5_drive.send(s, l);
		usleep(1000000);
		l=snprintf((char*)&s[0], network_mtu, "disc2net=on;\n");
		m5_drive.send(s, l);
		return;
	}
	// I am the parent.
	// 8.3.2 Receive incoming connection from Mark5
	m5_datab.listen();
	int sd=m5_datab.accept();
	TCPSocket m5_data(sd, network_mtu);

	// 9. Receive/Transmit data.
	bool active=true;
	while (active)
	{
		bytes_read=m5_data.recv(s, network_mtu);
		if (bytes_read<0) {
			error_logger << "Negative recv: " << bytes_read << ENDL;
			break;
		}
		if (bytes_read==0)
			continue;
		int bytes_sent=egae_data.send(s, bytes_read);	
	}

	// 10. Turn off local M5.
	int l=snprintf((char*)&s[0], network_mtu, "disc2net=disconnect;\n");
	m5_drive.send(s, l);

	// 11. Turn off remote egae.
	egae_data.shutdown(SHUT_WR);	
	bytes_read=egae_drive.recv(s, 1);
	debug_logger << "-server()\n";
}

void client(const Options& o)
{
	Logger& debug_logger=(*Logsystem::debug_logger());
	Logger& error_logger=(*Logsystem::error_logger());
	debug_logger << "client()\n";

    openlog("egaedrive", LOG_NDELAY, LOG_USER);
	syslog(LOG_INFO, "egaedrive");

	// Get port numbers.
	struct servent *service;
	service=getservbyname("egaedrive", "tcp");
	int egaedrive_port=service->s_port;
	service=getservbyname("egaedata", "tcp");
	int egaedata_port=service->s_port;
	service=getservbyname("m5drive", "tcp");
	int m5drive_port=service->s_port;
	service=getservbyname("m5data", "tcp");
	int m5data_port=service->s_port;

	syslog(LOG_INFO, "egaedrive: %d\n", ntohs(egaedrive_port));
	syslog(LOG_INFO, "egaedata:  %d\n", ntohs(egaedata_port));
	syslog(LOG_INFO, "m5drive:   %d\n", ntohs(m5drive_port));
	syslog(LOG_INFO, "m5data:    %d\n", ntohs(m5data_port));

	// Constants.
	const int network_mtu=o.get_network_mtu();

	// Buffers.
	SocketBuffer s;
	s.resize(network_mtu);

	// egae_drive socket is on stdin/stdout.
	char file_name[o.get_to_file().size()+1];
	strncpy(file_name, o.get_to_file().c_str(), o.get_to_file().size());
	syslog(LOG_INFO, "File name: %s\n", file_name);

	// 3. Connect to mark5_ctrl and set it up for reception.
	TCPSocket m5_drive(network_mtu);
	m5_drive.connect("127.0.0.1", m5drive_port);
	int l=snprintf((char*)&s[0], network_mtu, 
		"net2disc=open:%s;\n",
		file_name);
	m5_drive.send(s, l);
	syslog(LOG_INFO, "Sent to Mark5 control\n");

	// 4. Connect to mark5_data.
	TCPSocket m5_data(network_mtu);
	m5_data.connect("127.0.0.1", m5data_port);
	syslog(LOG_INFO, "Connected to Mark5 data\n");

	// 5. Bind to egae_data port.
	TCPSocket egae_datab(network_mtu);
	egae_datab.bind("0.0.0.0", egaedata_port);
	egae_datab.listen();

	// Need to fork() to be able to send command and receive
	// data at the same time.
	pid_t child_pid=fork();
	// 6.1 Respond to server egae_drive port. 
	if (child_pid==0) {
		// I am the child. Wait for parent to setup before sending command.
		usleep(5000000);
		char ack[] = "b3ack";
		int bytes_sent=fwrite((void*)ack, 1, 5, stdout);
		if (bytes_sent<0) 
			syslog(LOG_WARNING, "Unable to send ack\n");
		return;
	}
	// I am the parent.
	// 6.2 Accept incoming data connection from egae server.
	syslog(LOG_INFO, "In parent after fork\n");
	int sd=egae_datab.accept();
	TCPSocket egae_data(sd, network_mtu);

	syslog(LOG_INFO, "Accepted connection from egae server\n");

	// 7. Recv/Transmit data.
	bool active=true;
	while (active)
	{
		syslog(LOG_INFO, "Top of read loop\n");
		int bytes_read=egae_data.recv(s, network_mtu);
		if (bytes_read<=0) {
			syslog(LOG_WARNING, "Negative recv: %d\n",  bytes_read);
			break;
		}
		int bytes_sent=m5_data.send(s, bytes_read);	
	}
	// 12. Close down Mark5.
	l=snprintf((char*)&s[0], network_mtu, "net2disc=close;\n");
	m5_drive.send(s, l);
	syslog(LOG_INFO, "Sent close to Mark5\n");
	
	// 13. Ack FIN from server egae.
	char buf[network_mtu];
	int bytes_read=fread((void*)buf, 1, 1, stdin);
	syslog(LOG_INFO, "Received fin: %c\n", buf[0]);
	char fin[] = "c3ack";
	int bytes_sent=fwrite((void*)fin, 1, 3, stdout);
	if (bytes_sent<0) 
		syslog(LOG_WARNING, "Unable to send fin ack\n");
	egae_data.shutdown(SHUT_WR);	
	egae_data.close();
	closelog();
}

