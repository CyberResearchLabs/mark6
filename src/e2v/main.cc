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

void usage() {
  std::cout << "e2v <input file> <port list>" << std::endl;
}

// Only look at last two digits of port for demux.
const int MAX_FDS = 100;
char fds[MAX_FDS];

std::list<int> PORT_LIST;

int
main(int argc, char *argv[]) {
  if (argc < 4) {
    usage();
    return(-1);
  }

  std::string dev(argv[1]);
  std::string out(argv[2]);
  for (int i=3; i<argc; i++) {
    PORT_LIST.push_back(atoi((const char*)argv[i]));
  }

  BOOST_FOREACH(unsigned int port, PORT_LIST) {
    std::cout << port << " ";
    std::ostringstream oss (std::ostringstream::out);
    oss << out << "-" << port << ".vdif";
    std::cout << "Opening: " << oss.str() << std::endl;
    fds[port % MAX_FDS] =  ::open(oss.str().c_str(), O_WRONLY
				  | O_CREAT, S_IRWXU);
  }

  struct pcap_pkthdr header;
  const u_char* packet;

  std::cout << "Dev: " << dev << std::endl;
  std::cout << "Out: " << out << std::endl;

  //open the pcap file 
  pcap_t *handle; 
  char errbuf[PCAP_ERRBUF_SIZE];
  handle = pcap_open_offline(dev.c_str(), errbuf);
  if (handle == NULL) { 
    std::cout << "Couldn't open pcap file " << dev << " " << errbuf
	      << std::endl;
    return(-1); 
  }

  // Packet processing loop.
  while (packet = pcap_next(handle, &header)) {
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
    }
  } //end internal loop for reading packets (all in one file) 
 
  pcap_close(handle);  //close the pcap file 

  BOOST_FOREACH(unsigned int port, PORT_LIST) {
    close(fds[port % MAX_FDS]);
  }
  return(0);
}


