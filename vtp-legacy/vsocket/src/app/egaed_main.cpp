#include <common.h>
#include <Socket.h>
#include <Logsystem.h>
#include <Options.h>
#include <fstream>
#include <vector>
#include <syslog.h>
#include <unistd.h>
#include <netdb.h>

int
main(int argc, char** argv)
{
	Options o(argc, argv);
	string log_name(o.get_log_file());
	ofstream log(log_name.c_str(), ios::out | ios::trunc);
	const int network_mtu=o.get_network_mtu();
	openlog("egaedrive", LOG_NDELAY, LOG_USER);
	syslog(LOG_INFO, "Entering m5d");
	SocketBuffer buf(network_mtu+1);
	while (!feof(stdin)) {
		syslog(LOG_INFO, "Top of receive loop");
		// delimiter extracted but not stored. NULL added to 
		// end of character array. Maximum string size read is
		// network_mtu-1 (one place must be reserved for appended
		// NULL.
		int bytes_read=fread((void*)&(buf[0]), 1, network_mtu, stdin);
		syslog(LOG_INFO, "Received %d bytes", bytes_read);
		char cmd=buf[0];
		char length=buf[1];
		syslog(LOG_INFO, "Command type:   %c", cmd);
		syslog(LOG_INFO, "Message length: %d", length);
		string file_name;
		file_name.reserve(length+1);
		int i=0;
		for (i=2; i<2+length; i++)
			file_name+=buf[i];
		file_name+='\0';
		syslog(LOG_INFO, "File name: %s\n", (char*)file_name.c_str());
		switch (cmd) {
		case 'a':
			{
				char prog[o.get_exec().size()+1];
				strncpy(prog, o.get_exec().c_str(), o.get_exec().size());
				char arg1[] = "-c";
				char arg2[] = "-T";
				char arg3[length+1];
				strncpy(arg1, file_name.c_str(), length+1);
				char *args[5]={ prog, arg1, arg2, arg3, NULL };
				char *envp[1]={ NULL };
				execve(prog, args, envp);
				break;
			}
		default:
			break;
		}
	}
	syslog(LOG_INFO, "Leaving egaedrive");
	closelog();
	log.close();
}
