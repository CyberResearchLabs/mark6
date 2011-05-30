#include <VRSocketA.h>
#include <Options.h>
#include <Utils.h>
#include <File.h>
#include <stdio.h>
#include <deque>
#include <unistd.h>
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
	default:
		break;
	}
	switch (o.get_debug_level()) {
	case 3:
    	setlogmask(LOG_UPTO(LOG_DEBUG));
		break;
	case 2:
    	setlogmask(LOG_UPTO(LOG_INFO));
		break;
	case 1:
    	setlogmask(LOG_UPTO(LOG_WARNING));
		break;
	case 0:
    	setlogmask(LOG_UPTO(LOG_ERR));
		break;
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
		default:
			(*Logsystem::debug_logger()) << "Uknown test mode\n";
			break;
		}
	} catch (exception e) {
		(*Logsystem::debug_logger()) << "exception: " << e.what() << "\n";
	}
	return(0);
}

void client(const Options& o)
{
	cout << "client()\n";
	cout << "    remote_ip==" << o.get_remote_ip() << "\n";
	cout << "    host_mtu==" << o.get_host_mtu() << "\n";
	cout << "    network_mtu==" << o.get_network_mtu() << "\n";
	cout << "    rtt==" << o.get_rtt() << "\n";
	cout << "    peak_rate==" << o.get_peak_rate() << "\n";
    cout << "    tvg==" << o.get_tvg() << "\n";
    cout << "    tvg_sz==" << o.get_tvg_sz() << "\n";

	// Constants.
	const int host_mtu=o.get_host_mtu();
	const int network_mtu=o.get_network_mtu();
	const int udp_port=49000;
	const int tcp_port=2630;
	const string& ip=o.get_remote_ip();
	TSpec traffic_spec;
	traffic_spec._peak_rate=o.get_peak_rate();
	const bool tvg=o.get_tvg();
	const int tvg_sz=o.get_tvg_sz();
	// Socket creation and connection.
	VRSocketA p(VRSocketA::receiver, network_mtu, traffic_spec, ip, 
				udp_port, tcp_port, tvg, tvg_sz);
	p.join(); // Wait for transmission  to end.
	p.close();
	cout << "-client()\n";
}

void server(const Options& o)
{
	cout << "server()\n";
	cout << "    remote_ip==" << o.get_remote_ip() << "\n";
	cout << "    host_mtu==" << o.get_host_mtu() << "\n";
	cout << "    network_mtu==" << o.get_network_mtu() << "\n";
	cout << "    rtt==" << o.get_rtt() << "\n";
	cout << "    peak_rate==" << o.get_peak_rate() << "\n";
    cout << "    tvg==" << o.get_tvg() << "\n";
    cout << "    tvg_sz==" << o.get_tvg_sz() << "\n";

	// Constants.
	const int host_mtu=o.get_host_mtu();
	const int network_mtu=o.get_network_mtu();
	const int udp_port=49000;
	const int tcp_port=2630;
	const string& ip=o.get_remote_ip();
	TSpec traffic_spec;
	traffic_spec._peak_rate=o.get_peak_rate();
	const bool tvg=o.get_tvg();
	const int tvg_sz=o.get_tvg_sz();
	VRSocketA p(VRSocketA::sender, network_mtu, traffic_spec, ip,
				udp_port, tcp_port, tvg, tvg_sz);
	Timer total;
	total.start();
	usleep(1000000);	// Wait for thread to start.
	p.join();	// Wait for EOT.
	total.stop();
	cout << "Total time(s)==" << total.elapsed() << "\n";
	p.close();
	cout << "-server()\n";
}
