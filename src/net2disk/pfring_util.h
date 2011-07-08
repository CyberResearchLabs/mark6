
#ifndef _PFRING_UTIL_H_
#define _PFRING_UTIL_H_

// C includes.
#include <sys/types.h>

// C++ includes.
#include <string>

extern int verbose;
extern u_int32_t thiszone;

extern char* _intoa(unsigned int addr, char* buf, u_short bufLen);
char* intoa(unsigned int addr);
extern std::string proto2str(u_short proto);
extern double delta_time (struct timeval * now, struct timeval * before);
extern int32_t gmt2local(time_t t);
extern int bind2core(u_int core_id);
extern char* etheraddr_string(const u_char *ep, char *buf);

#endif // _PFRING_UTIL_H_
