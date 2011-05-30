#include <VRServerSocket.h>
#include <VRClientSocket.h>
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
  switch (o._debug_level) {
  case 3:
    Logsystem::debug_logger()->enable_logging();
    setlogmask(LOG_UPTO(LOG_DEBUG));
  case 2:
    Logsystem::info_logger()->enable_logging();
    setlogmask(LOG_UPTO(LOG_INFO));
  case 1:
    Logsystem::warning_logger()->enable_logging();
    setlogmask(LOG_UPTO(LOG_WARNING));
  case 0:
    Logsystem::error_logger()->enable_logging();
    setlogmask(LOG_UPTO(LOG_ERR));
  default:
    break;
  }

  try {
    switch (o._mode) {
    case FILE2NET:
      openlog("vsocket_file2net", LOG_NDELAY, LOG_USER);
      server(o);
      break;
    case NET2FILE:
      openlog("vsocket_net2file", LOG_NDELAY, LOG_USER);
      client(o);
      break;
    case IN2NET:
      openlog("vsocket_in2net", LOG_NDELAY, LOG_USER);
      server(o);
      break;
    case NET2OUT:
      openlog("vsocket_net2out", LOG_NDELAY, LOG_USER);
      client(o);
      break;
    case TVG2NET:
      openlog("vsocket_tvg2net", LOG_NDELAY, LOG_USER);
      server(o);
      break;
    case NET2TVG:
      openlog("vsocket_net2tvg", LOG_NDELAY, LOG_USER);
      client(o);
      break;;
    default:
      (*Logsystem::debug_logger()) << "Uknown test mode\n";
      break;
    }
  } catch (exception e) {
    (*Logsystem::debug_logger()) << "exception: " << e.what() << "\n";
  }
  closelog();
  return(0);
}

void client(const Options& o)
{
  cout << "client()\n";
  o.dump(cout);

  // Constants.
  VRClientSocket c(o._mode, o._tspec, o._remote_ip, o._data_port, o._ctrl_port, 
                   tvg_duration, o._to_file, o._max_window);
  c.run();
  cout << "-client()\n";
}

void server(const Options& o)
{
  cout << "server()\n";
  o.dump(cout);

  // Constants.
  const int host_mtu=o._host_mtu;
  const int network_mtu=o._network_mtu;
  const int udp_port=49000;
  const int tcp_port=2630;
  const int ctrl_port=49001;
  const string& ip=o._remote_ip;
  TSpec traffic_spec;
  traffic_spec._peak_rate=o._tspec._peak_rate;
  traffic_spec._mtu=o._network_mtu;
  traffic_spec._cc=o._tspec._cc;
  traffic_spec._min_rate=1000;
  const bool tvg=o._tvg;
  const double tvg_duration=o._tvg_duration;
  const u_int32_t sampling_frequency=o._sampling_frequency;
  const u_int32_t bits_per_sample=o._bits_per_sample;
  const u_int32_t packet_size=o._packet_size;
  const string& from_file=o._from_file;
  const u_int16_t max_window=o._max_window;
  const u_int32_t max_bucket=o._max_bucket;
  VRServerSocket s(traffic_spec, ip, udp_port, tcp_port, 
                   ctrl_port, tvg, tvg_duration, sampling_frequency, 
                   packet_size, bits_per_sample, from_file, o._scan_vector,
                   max_window, max_bucket);
  Timer total;
  total.start();
  s.run();	// Wait for EOT.
  total.stop();
  cout << "Total time(s)==" << total.elapsed() << "\n";
  cout << "-server()\n";
}

