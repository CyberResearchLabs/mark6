#ifndef DISK2VLBI_H
#define DISK2VLBI_H

#include <string>

#define ETHER_TYPE_IP (0x0800)
#define ETHER_TYPE_8021Q (0x8100)
#define UDP_PROTOCOL_NUMBER (17)
#define UDP_HEADER_LENGTH (8)

class Disk2vlbi {
 public:
  Disk2vlbi(const std::string capture_file);
};

#endif // DISK2VLBI_H
