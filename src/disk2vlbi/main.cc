// C includes.
#include <stdlib.h>
#include <iostream>
#include <stdio.h>
#include <pcap.h>
#include <netinet/ip.h>
#include <netinet/udp.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

//C++ includes
#include <list>
#include <sstream>
#include <string>

// Framework includes.
#include <boost/foreach.hpp>
#include <boost/program_options.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/algorithm/string.hpp> 

// Namespaces.
namespace po = boost::program_options;

#define ETHER_TYPE_IP (0x0800)
#define ETHER_TYPE_8021Q (0x8100)
#define UDP_PROTOCOL_NUMBER (17)
#define UDP_HEADER_LENGTH (8)

void
usage(const po::options_description& desc) {
  std::cout
    << "disk2vlbi [options]" << std::endl
    << desc;
}

// Only look at last two digits of port for demux.
const int MAX_FDS = 100;
char fds[MAX_FDS];

int
main(int argc, char *argv[]) {
  // Variables to store options.
  std::string input_file;
  std::string output_prefix;
  std::vector<int> thread_ports;
  unsigned int size;

  // Declare supported options, defaults, and variable bindings.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help", "produce help message")
    ("input_file",
     po::value<std::string>(&input_file),
     "input file")
    ("output_prefix",
     po::value<std::string>(&output_prefix),
     "output prefix")
    ("thread_ports",
     po::value< std::vector<int> >(&thread_ports)->multitoken(),
     "thread udp ports")
    ("size",
     po::value<unsigned int>(&size),
     ("Total number of megabytes to output (across all files).")
     );


  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, desc), vm);
  po::notify(vm);

  if (vm.count("help") || argc < 4) {
    usage(desc);
    return 1;
  }
  
  BOOST_FOREACH(int port, thread_ports) {
    std::cout << port << " ";
    std::ostringstream oss (std::ostringstream::out);
    oss << output_prefix << "-" << port << ".vdif";
    std::cout << "Opening: " << oss.str() << std::endl;
    fds[port % MAX_FDS] =  ::open(oss.str().c_str(), O_WRONLY
				  | O_CREAT, S_IRWXU);
  }

  struct pcap_pkthdr header;
  const u_char* packet;

  //open the pcap file 
  pcap_t *handle; 
  char errbuf[PCAP_ERRBUF_SIZE];
  handle = pcap_open_offline(input_file.c_str(), errbuf);
  if (handle == NULL) { 
    std::cout << "Couldn't open pcap file " << input_file
	      << " " << errbuf
	      << std::endl;
    return(-1); 
  }

  // Packet processing loop.
  unsigned int bytes_written = 0;
  unsigned int mbytes_written = 0;
  while ( (packet = pcap_next(handle, &header)) && (mbytes_written < size) ) {
    u_char *pkt_ptr = (u_char *)packet;
    int ether_type = ((int)(pkt_ptr[12]) << 8) | (int)pkt_ptr[13]; 
    int ether_offset = 0; 
    if (ether_type == ETHER_TYPE_IP)
      ether_offset = 14; 
    else if (ether_type == ETHER_TYPE_8021Q)
      ether_offset = 18; 
    else 
      printf("Unknown ethernet type, %04X, skipping...\n", ether_type);
 
    //parse the IP header 
    pkt_ptr += ether_offset;
    struct ip *ip_hdr = (struct ip *)pkt_ptr;
    const int header_length = ip_hdr->ip_hl*4;
    const int packet_length = ntohs(ip_hdr->ip_len);
    if (ip_hdr->ip_p == UDP_PROTOCOL_NUMBER) {
      pkt_ptr += header_length;
      struct udphdr* udp_hdr = (struct udphdr*)pkt_ptr;
      const int dport = ntohs(udp_hdr->dest);
      const int udp_length = ntohs(udp_hdr->len) - UDP_HEADER_LENGTH;
      pkt_ptr += UDP_HEADER_LENGTH;
      u_char* data = pkt_ptr;
      int nb = write(fds[dport%MAX_FDS], data, udp_length);
      bytes_written += nb;
      if (bytes_written >= 1048576) {
	mbytes_written++;
	bytes_written = 0;
      }
    }
  } //end internal loop for reading packets (all in one file) 
 
  pcap_close(handle);  //close the pcap file 

  BOOST_FOREACH(unsigned int port, thread_ports) {
    close(fds[port % MAX_FDS]);
  }
  return(0);
}


