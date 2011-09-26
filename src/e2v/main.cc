#include <iostream>
#include <stdio.h>
#include <pcap.h>
#include <netinet/ip.h>
#include <netinet/udp.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define ETHER_TYPE_IP (0x0800)
#define ETHER_TYPE_8021Q (0x8100)
#define UDP_PROTOCOL_NUMBER (17)
#define UDP_HEADER_LENGTH (8)

int
main(int argc, char *argv[]) {
  char *dev = argv[1];
  char *out = argv[2];
  struct pcap_pkthdr header;
  const u_char* packet;

  std::cout << "Dev: " << dev << std::endl;
  std::cout << "Out: " << out << std::endl;

  int out_fd = ::open(out, O_WRONLY | O_CREAT, S_IRWXU);

  //open the pcap file 
  pcap_t *handle; 
  char errbuf[PCAP_ERRBUF_SIZE]; //not sure what to do with this, oh well 
  handle = pcap_open_offline(dev, errbuf);   //call pcap library function 
  if (handle == NULL) { 
    std::cout << "Couldn't open pcap file " << dev << " " << errbuf
	      << std::endl;
    return(-1); 
  }

  while (packet = pcap_next(handle, &header)) {
    u_char *pkt_ptr = (u_char *)packet;
    int ether_type = ((int)(pkt_ptr[12]) << 8) | (int)pkt_ptr[13]; 
    int ether_offset = 0; 
    if (ether_type == ETHER_TYPE_IP) //most common 
      ether_offset = 14; 
    else if (ether_type == ETHER_TYPE_8021Q) //my traces have this 
      ether_offset = 18; 
    else 
      fprintf(stdout, "Unknown ethernet type, %04X, skipping...\n",
	      ether_type); 
 
    //parse the IP header 
    pkt_ptr += ether_offset;  //skip past the Ethernet II header 
    struct ip *ip_hdr = (struct ip *)pkt_ptr; //point to an IP header structure    
    int header_length = ip_hdr->ip_hl*4;
    int packet_length = ntohs(ip_hdr->ip_len);
    if (ip_hdr->ip_p == UDP_PROTOCOL_NUMBER) {
      // std::cout << "Great " << header_length
      // << " " << packet_length << std::endl;
      pkt_ptr += header_length;
      struct udphdr* udp_hdr = (struct udphdr*)pkt_ptr;
      int udp_length = ntohs(udp_hdr->len) - UDP_HEADER_LENGTH;
      pkt_ptr += UDP_HEADER_LENGTH;
      u_char* data = pkt_ptr;
      int nb = write(out_fd, data, udp_length);
      // std::cout << nb << std::endl;
    }

  } //end internal loop for reading packets (all in one file) 
 
  pcap_close(handle);  //close the pcap file 
  close(out_fd);
  return(0);
}


