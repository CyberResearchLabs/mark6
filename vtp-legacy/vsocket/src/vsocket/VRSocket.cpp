/*
 *  VRSocket.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <VRSocket.h>
#include <Utils.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/times.h>		// For times().
#include <sys/resource.h>	// For getrusage().
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iostream>
#include <stdio.h>
#include <errno.h>
#include <sys/uio.h>
#include <unistd.h>			// For getrusage().
#include <syslog.h>


VRSocket::VRSocket(const TSpec& t):
  _rtp_session(t)
{
  START_FUNC("VRSocket")
    _state=PS_CONN_INIT;
  _udp_sock.set_so_reuseaddr(1);
  _tcp_sock.set_so_reuseaddr(1);
  _tcp_accept_sock.set_so_reuseaddr(1);
  _t_spec._peak_rate=DEFAULT_PEAK_RATE;
  _max_window=DEFAULT_WINDOW_SIZE;
  _tvg=false;
  _tvg_duration=0;
  cout << "    <VRSocket>\n";
  cout << "    </VRSocket>\n";
  END_FUNC("VRSocket")
    }

VRSocket::VRSocket(const TSpec& t, const string& ip, const int& uport, 
                   const int& tport, const int& cport, const bool& tvg, const double& tvg_duration, 
                   const ScanVector& scan_vector, const u_int16_t& max_window):
  _rtp_session(t), _t_spec(t), _tvg(tvg), _tvg_duration(tvg_duration),
  _scan_vector(scan_vector), _max_window(max_window)
{
  START_FUNC("VRSocket")
    _state=PS_CONN_INIT;
  _udp_sock.set_so_reuseaddr(1);
  _tcp_sock.set_so_reuseaddr(1);
  _tcp_accept_sock.set_so_reuseaddr(1);
  _udp_port=uport;
  _tcp_port=tport;
  _ctrl_port=cport;
  _remote_ip=ip;
  ScanVector::const_iterator iter;
  cout << "    <VRSocket>\n";
  for (iter=_scan_vector.begin(); iter!=_scan_vector.end(); ++iter) {
    cout << "        <scan_file>\n";
    cout << "            <_value>" << iter->_scan_file << "</_value>\n";
    cout << "            <_size>" << iter->_size << "</_size>\n";
    cout << "            <_date>" << iter->_size << "</_date>\n";
    cout << "            <_src>" << iter->_size << "</_src>\n";
    cout << "        </scan_file>\n";
  }
  cout << "    </VRSocket>\n";
  END_FUNC("VRSocket")
    }

VRSocket::VRSocket(const VRSocket &s):
  _rtp_session(s._t_spec)
{
  START_FUNC("VRSocket")
    _state=s._state;
  _t_spec=s._t_spec;
  _udp_sock=s._udp_sock;
  _tcp_sock=s._tcp_sock;
  _udp_sock.set_so_reuseaddr(1);
  _tcp_sock.set_so_reuseaddr(1);
  _tcp_accept_sock.set_so_reuseaddr(1);
  _tvg=s._tvg;
  _tvg_duration=s._tvg_duration;
  _rtp_session=s._rtp_session;
  _scan_vector=s._scan_vector;
  END_FUNC("VRSocket");
}

VRSocket::~VRSocket()
{
  START_FUNC("VRSocket")
    // This is an important feature of the VRSocket class. Especially
    // for copying of VRSockets. VRSockets do *not* close their
    // sockets when they are destroyed.
    END_FUNC("VRSocket")
    }

VRSocket& VRSocket::operator=(const VRSocket& s)
{
  START_FUNC("VRSocket")
    _t_spec=s._t_spec;
  _udp_sock=s._udp_sock;
  _tcp_sock=s._tcp_sock;
  _udp_sock.set_so_reuseaddr(1);
  _tcp_sock.set_so_reuseaddr(1);
  _tcp_accept_sock.set_so_reuseaddr(1);
  END_FUNC("VRSocket")
    return(*this);
}

ostream& operator<<(ostream& os, const VRSocket& s)
{
  os << "VRSocket dump\n";
  os << "-VRSocket dump\n";
  return(os);
}

void VRSocket::print_rusage()
{
  struct rusage usage;
  if (getrusage(RUSAGE_SELF, &usage)!=0) {
    syslog(LOG_ERR, "Unable to get resource usage\n");
    printf("Unable to get resource usage\n");
    return;
  }
  printf("<RUSAGE>\n");
  printf("    <ru_utime> %f <ru_utime>\n",  usage.ru_utime.tv_sec+
         usage.ru_utime.tv_usec/1e6);
  printf("    <ru_stime> %f <ru_stime>\n",  usage.ru_stime.tv_sec+
         usage.ru_stime.tv_usec/1e6);
  printf("    <ru_maxrss> %ld <ru_maxrss>\n", usage.ru_maxrss);
  printf("    <ru_ixrss> %ld <ru_ixrss>\n", usage.ru_ixrss);
  printf("    <ru_idrss> %ld <ru_idrss>\n", usage.ru_idrss);
  printf("    <ru_isrss> %ld <ru_isrss>\n", usage.ru_isrss);
  printf("    <ru_minflt> %ld <ru_minflt>\n", usage.ru_minflt);
  printf("    <ru_majflt> %ld <ru_majflt>\n", usage.ru_majflt);
  printf("    <ru_nswap> %ld <ru_nswap>\n", usage.ru_nswap);
  printf("    <ru_inblock> %ld <ru_inblock>\n", usage.ru_inblock);
  printf("    <ru_oublock> %ld <ru_oublock>\n", usage.ru_oublock);
  printf("    <ru_msgsnd> %ld <ru_msgsnd>\n", usage.ru_msgsnd);
  printf("    <ru_msgrcv> %ld <ru_msgrcv>\n", usage.ru_msgrcv);
  printf("    <ru_nsignals> %ld <ru_nsignals>\n", usage.ru_nsignals);
  printf("    <ru_nvcsw> %ld <ru_nvcsw>\n", usage.ru_nvcsw);
  printf("    <ru_nivcsw> %ld <ru_nivcsw>\n", usage.ru_nivcsw);
  printf("</RUSAGE>\n");
}

bool VRSocket::test()
{
  pid_t child_id=fork();
  bool ret=false;
  if (child_id==0)
    ; // ret=parent();
  else
    ; // ret=child();
  return(ret);
}

