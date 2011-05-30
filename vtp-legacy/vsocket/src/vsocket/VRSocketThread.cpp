/*
 *  VRSocketThread.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <VRSocketThread.h>
#include <Utils.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iostream>
#include <errno.h>


const int VRSocketThread::receiver=0;
const int VRSocketThread::sender=1;

void* VRSocketThread::thread_func()
{
  START_FUNC("VRSocketThread")
    switch (_mode) {
    case VRSocketThread::receiver:
      recv_proc();
      break;
    case VRSocketThread::sender:
      send_proc();
      break;
    default:
      DEBUG_FUNC("VRSocketThread") << "Unknown _mode: " << _mode << "\n"; 
    }
  return(NULL);
  END_FUNC("VRSocketThread")
    }

void VRSocketThread::recv_proc()
{
  START_FUNC("VRSocketThread")
    struct timeval to;
  to.tv_sec=2;
  to.tv_usec=0;
  int total_bytes_rcvd=0;
  int data_bytes_rcvd=0;
  const int hdr_size=6;
  int length=0, sn=0, ret=0;
  vector<SocketBufferCtrlBlk> sbcb_vec;
  SocketBufferCtrlBlk sbcb_hdr;
  int rn=0;
  Timer ack_timer;
  const double ack_timeout=0.1; 	// seconds.
  while (true) {
    sbcb_vec.clear();
    switch (_state) {
    case PS_INIT:
      {
        _state=PS_RX_DATA;
        ack_timer.start();
      }
      break;
    case PS_RX_DATA:
      {
        WARNING_FUNC("VRSocketThread") << "\n---- PS_RX_DATA ----\n";
        // Prepare the buffers.
        sbcb_vec.resize(2);
        sbcb_vec[0]._sb.resize(hdr_size);
        sbcb_vec[1]._sb.resize(_mtu);
        sn=-1;
        // Check for socket availability:
        // if (_data_sock.select(Socket::SEL_READ, 2.1)) {
        // FIXME
        if (_data_sock.select(Socket::SEL_READ, 0.0)) {
          ERROR_FUNC("VRSocketThread") << "ready for reading.\n";
          // Read data.
          ret=_data_sock.recv(sbcb_vec);
        } else {
          ERROR_FUNC("VRSocketThread") << "_data_sock not ready for rdg\n";
          if (data_bytes_rcvd>0)
            _state=PS_TX_ACK;
          continue;
        }
        if (ret<hdr_size) {
          if (data_bytes_rcvd>0)
            _state=PS_TX_ACK;
          ERROR_FUNC("VRSocketThread") << "Invalid recv(): ret==" 
                                       << ret << ENDL;
          continue;
        }
        // Valid length read.
        total_bytes_rcvd+=ret;
        // Process the packet.
        unsigned char* hdr=&(sbcb_vec[0]._sb[0]);
        bool fin=false;
        switch (hdr[1]) {
        case PT_DATA:
          INFO_FUNC("VRSocketThread") << "Received PT_DATA\n";
          break;
        case PT_FIN:
          INFO_FUNC("VRSocketThread") << "Received PT_FIN\n";
          _state=PS_BYE;
          fin=true;
          break;
        default:
          ERROR_FUNC("VRSocketThread") << "Unknown packet type\n";
          continue;
        }
        if (fin)
          continue;
        length=(hdr[2]<<8)+(hdr[3]);
        sn=(hdr[4]<<8)+(hdr[5]);
        data_bytes_rcvd+=length;
        print_pkt_hdr(sbcb_vec[0]._sb);
        DEBUG_FUNC("VRSocketThread") 
          << " total bytes rcvd: " << total_bytes_rcvd << "\n";
        DEBUG_FUNC("VRSocketThread") 
          << " data bytes rcvd: " << data_bytes_rcvd << "\n";
        WARNING_FUNC("VRSocketThread") 
          << " rcvd DATA with sn==" << sn << "\n";
        if (ret!=length+hdr_size) {
          ERROR_FUNC("VRSocketThread") << "short read: " << ret << "\n";
          ERROR_FUNC("VRSocketThread") << "length: " << length << "\n";
        }
        sbcb_vec[1]._bytes_read=length;
        // Update sequence number.
        if (sn==rn) {
          WARNING_FUNC("VRSocketThread") << "Accept packet sn==" 
                                         << sn << ENDL;
          _recv_mutex.lock();
          _recv_sockbufctrlblk_map[sn]=sbcb_vec[1];
          _recv_mutex.unlock();
          rn++;
        } else {
          WARNING_FUNC("VRSocketThread") << "Discard packet sn==" 
                                         << sn << ENDL;
        }
        // Did we receive any data packets?
        ack_timer.stop();
        if (ack_timer.elapsed()>ack_timeout && data_bytes_rcvd>0)
          _state=PS_TX_ACK;
      }
      break;
    case PS_TX_ACK:
      {
        WARNING_FUNC("VRSocketThread") << "\n---- PS_TX_ACK ----\n";
        // Build ack.
        // FIXME
        // sbcb_hdr._sb.resize(ack_size);
        sbcb_hdr._sb.clear();
        sbcb_hdr._sb.resize(_mtu);
        if (build_ack_hdr(sbcb_hdr._sb, rn, hdr_size)!=0)
          ERROR_FUNC("VRSocketThread") << "Unable to build ACK header\n";
        INFO_FUNC("VRSocketThread") << "Ack Header\n";
        print_pkt_hdr(sbcb_hdr._sb);
        // Load sbcb_vec.
        sbcb_vec.clear();
        sbcb_vec.resize(1);
        sbcb_vec.push_back(sbcb_hdr);
        // Send!
        WARNING_FUNC("VRSocketThread") << "About to send ack!!!\n";
        ret=_ctrl_sock.send(sbcb_vec);
        if (ret!=(int)sbcb_hdr._sb.size())
          ERROR_FUNC("VRSocketThread") << "Failed to send ack!!!\n";
        else {
          WARNING_FUNC("VRSocketThread") << "Sent ack!!!==" << rn << ENDL;
          // Restart ack timer.
          ack_timer.start();
        }
        sbcb_vec.pop_back();
        _state=PS_RX_DATA;
      }
      break;
    case PS_BYE:
      {
        WARNING_FUNC("VRSocketThread") << "\n---- PS_BYE ----\n";
        bool fin=false;
        while (fin!=true)
          {
            sbcb_vec.clear();
            sbcb_hdr._sb.clear();
            sbcb_hdr._sb.resize(_mtu);
            if (build_fin_hdr(sbcb_hdr._sb, hdr_size)!=0) {
              ERROR_FUNC("VRSocketThread") 
                << "Unable to build FIN packet header\n";
              continue;
            }
            INFO_FUNC("VRSocketThread") << "FIN packet to be sent\n";
            print_pkt_hdr(sbcb_hdr._sb);
            sbcb_vec.push_back(sbcb_hdr);
            // Transmit data.
            ret=_ctrl_sock.send(sbcb_vec);
            if (ret<=0) {
              ERROR_FUNC("VRSocketThread") << "Failed to send FIN!!!\n";
              continue;
            }
            // Successfully transmitted (at least) part of the packet.
            if (_mtu-ret!=0)
              ERROR_FUNC("VRSocketThread") << "short fin-send: " << ret 
                                           << "\n";
            else
              fin=true;
          }
        _mode=PS_APP_BYE;
        return;
      }
      break;
    default:
      ERROR_FUNC("VRSocketThread") << "Uknown state\n";
      break;
    }
  }
  END_FUNC("VRSocketThread")
    }

void VRSocketThread::send_proc()
{
  START_FUNC("VRSocketThread")
    int total_bytes_sent=0;
  int data_bytes_sent=0;
  int rn=0;
  const int hdr_size=6;
  vector<SocketBufferCtrlBlk> sbcb_vec;
  SocketBufferCtrlBlk sbcb_hdr;
  SocketBufferCtrlBlk sbcb_data;
  int ret=0;
  unsigned char* hdr=NULL;
  unsigned int sn_max=0, sn_min=0, n=10;
  Timer ack_timer, send_timer;
  const double ack_timeout=0.1; 	// seconds.
  const double send_timeout=10.0; // seconds.
  const double retx_timeout=0.1;
  while (true) {
    switch (_state) {
    case PS_INIT:
      {
        WARNING_FUNC("VRSocketThread") << "\n---- PS_INIT ----\n";
        // FIXME
        // _ctrl_sock.set_blocking(Socket::NONBLOCKING);
        _state=PS_TX_DATA;
        ack_timer.start();
        send_timer.start();
        // FIXME Give sendbuf time to fill up...
        usleep(3000000);
      }
      break;
    case PS_TX_DATA:
      {
        WARNING_FUNC("VRSocketThread") << "\n---- PS_TX_DATA ----\n";
        if ((sn_max-sn_min)>=n) {
          _state=PS_RX_ACK;
          continue;
        }
        // Note! _send_mutex must always be acquired 
        // BEFORE _outstanding_mutex to avoid deadlock.
#if 0
        _send_mutex.lock();
        _outstanding_mutex.lock();
        if (_send_sockbufctrlblk_list.empty()) {
          if (_outstanding_sockbufctrlblk_map.empty()) {
            --timeout;
          } else {
            if (timeout)
              _state=PS_RETX;
            else
              _state=PS_BYE;
            WARNING_FUNC("VRSocketThread") << "timeout==" 
                                           << timeout << "\n";
            WARNING_FUNC("VRSocketThread") << "outsize==" 
                                           << (int)_outstanding_sockbufctrlblk_map.size() 
                                           << "\n";
          }
          _outstanding_mutex.unlock();
          _send_mutex.unlock();
          continue;
        }
        _outstanding_mutex.unlock();
        _send_mutex.unlock();
#else 
        // FIXME Need to work out why outsize>0
        _send_mutex.lock();
        bool send_buf_empty=_send_sockbufctrlblk_list.empty();
        _send_mutex.unlock();
        if ((sn_min==sn_max) && send_buf_empty) {
          send_timer.stop();
          if (send_timer.elapsed()>send_timeout) {
            _state=PS_BYE;
          } else {
            // FIXME Important, must ensure that state cycles.
            _state=PS_RX_ACK;
						
          }	
          WARNING_FUNC("VRSocketThread") << "outsize==" 
                                         << (int)_outstanding_sockbufctrlblk_map.size() 
                                         << "\n";
          continue;
        } 
        if (send_buf_empty) {
          // FIXME
          _state=PS_RX_ACK;
          WARNING_FUNC("VRSocketThread") << "outsize==" 
                                         << (int)_outstanding_sockbufctrlblk_map.size() 
                                         << "\n";
          continue;
        }
#endif 
        send_timer.start();
        // We have something to send.
        // Copy data from list into sbcb_data buffer.
        _send_mutex.lock();
        sbcb_data=*_send_sockbufctrlblk_list.begin();
        _send_mutex.unlock();
        sbcb_hdr._sb.resize(hdr_size);
        // Build data header.
        if (build_data_hdr(sbcb_hdr._sb, sn_max, 
                           hdr_size, sbcb_data._bytes_read)!=0)
          ERROR_FUNC("VRSocketThread") 
            << "Unable to build data packet header\n";
        INFO_FUNC("VRSocketThread") << "Packet to be sent\n";
        print_pkt_hdr(sbcb_hdr._sb);
        // Prepare sbcb_vec for send.
        sbcb_vec.clear();
        sbcb_vec.push_back(sbcb_hdr);
        sbcb_vec.push_back(sbcb_data);
        // Transmit data.
        ret=_data_sock.send(sbcb_vec);
        // FIXME
        if (ret<=0) {
          ERROR_FUNC("VRSocketThread") << "Failed send!!!\n";
          sbcb_vec.pop_back();
          sbcb_vec.pop_back();
          continue;
        }
        WARNING_FUNC("VRSocketThread") << "Sent bytes==" << ret << "\n";
        WARNING_FUNC("VRSocketThread") << "sn_min==" << sn_min << ": " 
                                       << "sn_max==" << sn_max << ENDL;
        // Successfully transmitted (at least) part of the packet.
        // Store buffer in outstanding map.
        if (ret!=(int)(sbcb_hdr._sb.size()+sbcb_data._sb.size()))
          ERROR_FUNC("VRSocketThread") 
            << "Unable to transmit entire packet\n";
        _send_mutex.lock();
        _outstanding_mutex.lock();
        _outstanding_sockbufctrlblk_map[sn_max++]=sbcb_data;
        // Start the clock ticking.
        sbcb_data._timer.start();
        _send_sockbufctrlblk_list.pop_front();
        _outstanding_mutex.unlock();
        _send_mutex.unlock();
        total_bytes_sent+=ret;
        data_bytes_sent+=ret-hdr_size;
        INFO_FUNC("VRSocketThread") << "total_bytes_sent==" 
                                    << total_bytes_sent << ":"
                                    << "data_bytes_sent==" 
                                    << data_bytes_sent << "\n";
        sbcb_data._bytes_sent+=ret;	
        if ((sbcb_vec[1]._bytes_read+hdr_size-ret)!=0)
          ERROR_FUNC("VRSocketThread") << "short send: " << ret << "\n";
        // Check for ack opportunity.
        // FIXME
        // ack_timer.stop();
        // if (ack_timer.elapsed()>ack_timeout)
        _state=PS_RX_ACK;		
      }
      break;
    case PS_RX_ACK:
      {
        WARNING_FUNC("VRSocketThread") << "\n---- PS_RX_ACK ----\n";
        SocketBufferCtrlBlk sbcb(_mtu);
        sbcb_vec.clear();
        // sbcb_vec.resize(1);
        sbcb_vec.push_back(sbcb);
        INFO_FUNC("VRSocketThread") << "sbcb_vec.size()==" << (int)sbcb_vec.size() << "\n";
        // Check for ACKs.
        ret=_ctrl_sock.recv(sbcb_vec);
        if (ret<=0) {
          WARNING_FUNC("VRSocketThread") << "Rcv ack failed\n";
          _state=PS_RETX;
          continue;
        }
        WARNING_FUNC("VRSocketThread") << "Received ctrl information.\n";
        // Received some data.
        hdr=&(sbcb_vec[0]._sb[0]);
        if (hdr[1]!=PT_ACK)
          ERROR_FUNC("VRSocketThread") << "Invalid ACK\n";
        else
          // Restart the ack timer.
          ack_timer.start();
        // Parse ACK.
        int length=(hdr[2]<<8)+hdr[3];
        rn=(hdr[4]<<8)+hdr[5];
        WARNING_FUNC("VRSocketThread") << "rn==" << rn  << ENDL;
        if ((rn-sn_min)<=(sn_max-sn_min)) {
          print_pkt_hdr(sbcb_vec[0]._sb);
          WARNING_FUNC("VRSocketThread") << "rn=" << rn 
                                         << "length=" << length << ENDL;
          _outstanding_mutex.lock();
          // FIXME
          // for (unsigned int i=sn_min; (i-sn_min)<=(rn-sn_min); i++) {
          for (unsigned int i=sn_min; (i-sn_min)<(rn-sn_min); i++) {
            _outstanding_sockbufctrlblk_map.erase(i);
          }
          _outstanding_mutex.unlock();
          sn_min=rn;
        }
        _state=PS_RETX;		
      }
      break;
    case PS_RETX:
      {
        WARNING_FUNC("VRSocketThread") << "\n---- PS_RETX ----\n";
        _outstanding_mutex.lock();
        if (_outstanding_sockbufctrlblk_map.empty()) {
          _outstanding_mutex.unlock();
          INFO_FUNC("VRSocketThread") << "Empty outstanding map\n";
          _state=PS_TX_DATA;
          continue;
        }
        _outstanding_mutex.unlock();
        // Ready for writing.
        for (unsigned int i=sn_min; (i-sn_min)<(sn_max-sn_min); i++) {
          _outstanding_mutex.lock();
          SocketBufferCtrlBlk& sbcb_datar=_outstanding_sockbufctrlblk_map[i];
          if (sbcb_datar._sb.size()==0) {
            ERROR_FUNC("VRSocketThread") 
              << "Empty out_standing buffer: sn==" << i << "\n";
            _outstanding_mutex.unlock();
            continue;
          } 
          // fIXME.
          sbcb_datar._timer.stop();
          if (sbcb_datar._timer.elapsed()<retx_timeout) {
            _outstanding_mutex.unlock();
            continue;
          }
          sbcb_vec.clear();
          sbcb_hdr._sb.clear();
          sbcb_hdr._sb.resize(hdr_size);
          if (build_data_hdr(sbcb_hdr._sb, i, hdr_size, 
                             sbcb_data._bytes_read)!=0)
            ERROR_FUNC("VRSocketThread") 
              << "Unable to build data packet header\n";
          INFO_FUNC("VRSocketThread") << "Packet to be sent\n";
          print_pkt_hdr(sbcb_hdr._sb);
          sbcb_vec.push_back(sbcb_hdr);
          sbcb_vec.push_back(sbcb_datar);
          INFO_FUNC("VRSocketThread") << "sbcb_vec[0].size(): " 
                                      << (int)sbcb_vec[0]._sb.size() << "\n";
          INFO_FUNC("VRSocketThread") << "sbcb_vec[1].size(): " 
                                      << (int)sbcb_vec[1]._sb.size() << "\n";
          // Transmit data.
          ret=_data_sock.send(sbcb_vec);
          WARNING_FUNC("VRSocketThread") << "outsize==" << 
            (int)_outstanding_sockbufctrlblk_map.size() << ENDL;
          _outstanding_mutex.unlock();
          if (ret<=0) {
            ERROR_FUNC("VRSocketThread") << "Failed re-send!!!\n";
            continue;
          } else {
            WARNING_FUNC("VRSocketThread") << "Resent sn==" << i << ENDL;
          }
          // Successfully transmitted (at least) part of the packet.
          total_bytes_sent+=ret;
          data_bytes_sent+=ret-hdr_size;
          if (_mtu+hdr_size-ret!=0)
            INFO_FUNC("VRSocketThread") << "short re-send: " << ret 
                                        << "\n";
          sbcb_vec.pop_back();
          sbcb_vec.pop_back();
        }
        _state=PS_TX_DATA;		
      }
      break;
    case PS_BYE:
      {
        WARNING_FUNC("VRSocketThread") << "\n---- PS_BYE ----\n";
        bool fin=false;
        while (fin!=true)
          {
            WARNING_FUNC("VRSocketThread") 
              << "FIN loop\n";
            sbcb_vec.clear();
            sbcb_hdr._sb.clear();
            sbcb_hdr._sb.resize(_mtu);
            if (build_fin_hdr(sbcb_hdr._sb, hdr_size)!=0) {
              ERROR_FUNC("VRSocketThread") 
                << "Unable to build FIN packet header\n";
              continue;
            }
            INFO_FUNC("VRSocketThread") << "FIN packet to be sent\n";
            print_pkt_hdr(sbcb_hdr._sb);
            sbcb_vec.push_back(sbcb_hdr);
            // Transmit data.
            ret=_data_sock.send(sbcb_vec);
            if (ret<=0) {
              ERROR_FUNC("VRSocketThread") << "Failed re-send!!!\n";
              continue;
            }
            // Successfully transmitted (at least) part of the packet.
            if (_mtu-ret!=0)
              ERROR_FUNC("VRSocketThread") << "short fin-send: "
                                           << ret << "\n";
            // Wait for acknowledgement from receiver.
            sbcb_vec.clear();
            sbcb_vec.resize(1);
            sbcb_vec[0]._sb.resize(_mtu);
            INFO_FUNC("VRSocketThread") << "sbcb_vec.size()==" 
                                        << (int)sbcb_vec.size() << "\n";
            // Check for ACKs.
            ret=_ctrl_sock.recv(sbcb_vec);
            if (ret<=0) {
              INFO_FUNC("VRSocketThread") << "No FIN received.\n";
              continue;
            }
            hdr=&(sbcb_vec[0]._sb[0]);
            if (hdr[1]==PT_FIN) {
              INFO_FUNC("VRSocketThread") << "Received FIN!\n";
              fin=true;
            } else {
              INFO_FUNC("VRSocketThread") << "Did not receive FIN!\n";
            }
          }
        _mode=PS_APP_BYE;
        return;
      }
      break;
    default:
      ERROR_FUNC("VRSocketThread") << "Uknown state\n";
      break;
    }
  }
  END_FUNC("VRSocketThread")
    }

VRSocketThread::VRSocketThread(const int& mode):
  _mode(mode), _mtu(DEFAULT_MTU)
{
  START_FUNC("VRSocketThread")
    _state=PS_INIT;
  _first_rx_sn=_last_rx_sn=_first_tx_sn=_last_tx_sn=0;
  _data_sock.set_so_reuseaddr(1);
  _ctrl_sock.set_so_reuseaddr(1);
  _ctrl_accept_sock.set_so_reuseaddr(1);
  thread_create();
  END_FUNC("VRSocketThread")
    }

VRSocketThread::VRSocketThread(const int& mode, const int& m):
  _mode(mode), _mtu(m)
{
  START_FUNC("VRSocketThread")
    _state=PS_INIT;
  _first_rx_sn=_last_rx_sn=_first_tx_sn=_last_tx_sn=0;
  _data_sock.set_so_reuseaddr(1);
  _ctrl_sock.set_so_reuseaddr(1);
  _ctrl_accept_sock.set_so_reuseaddr(1);
  thread_create();
  END_FUNC("VRSocketThread")
    }

VRSocketThread::VRSocketThread(const int& mode, const int& s, const int& m):
  _mode(mode), _mtu(m)
{
  START_FUNC("VRSocketThread")
    _state=PS_INIT;
  _first_rx_sn=_last_rx_sn=_first_tx_sn=_last_tx_sn=0;
  _data_sock.set_so_reuseaddr(1);
  _ctrl_sock.set_so_reuseaddr(1);
  _ctrl_accept_sock.set_so_reuseaddr(1);
  thread_create();
  END_FUNC("VRSocketThread")
    }

VRSocketThread::VRSocketThread(const VRSocketThread &s)
{
  START_FUNC("VRSocketThread")
    _first_rx_sn=s._first_rx_sn;
  _last_rx_sn=s._last_rx_sn;
  _first_tx_sn=s._first_tx_sn;
  _last_tx_sn=s._last_tx_sn;
  _mtu=s._mtu;
  _state=s._state;
  _data_sock=s._data_sock;
  _ctrl_sock=s._ctrl_sock;
  _data_sock.set_so_reuseaddr(1);
  _ctrl_sock.set_so_reuseaddr(1);
  _ctrl_accept_sock.set_so_reuseaddr(1);
  thread_create();
  END_FUNC("VRSocketThread")
    }

VRSocketThread::~VRSocketThread()
{
  START_FUNC("VRSocketThread")
    // This is an important feature of the VRSocketThread class. Especially
    // for copying of VRSocketThreads. VRSocketThreads do *not* close their
    // sockets when they are destroyed.
    END_FUNC("VRSocketThread")
    }

VRSocketThread& VRSocketThread::operator=(const VRSocketThread& s)
{
  START_FUNC("VRSocketThread")
    _mtu=s._mtu;
  _data_sock=s._data_sock;
  _ctrl_sock=s._ctrl_sock;
  _data_sock.set_so_reuseaddr(1);
  _ctrl_sock.set_so_reuseaddr(1);
  _ctrl_accept_sock.set_so_reuseaddr(1);
  END_FUNC("VRSocketThread")
    return(*this);
}

void VRSocketThread::bind(const string& ip, const int& dport, const int& cport)
{
  START_FUNC("VRSocketThread");
  _data_sock.bind(ip, dport);
  _ctrl_accept_sock.bind(ip, cport);
  END_FUNC("VRSocketThread");
}

void VRSocketThread::connect(const string& ip, const int& dport, const int& cport)
{
  START_FUNC("VRSocketThread");
  _data_sock.connect(ip, dport);
  // _ctrl_sock.set_blocking(Socket::NONBLOCKING);
  _ctrl_sock.connect(ip, cport);
  perror("Connect\n");
  while (!_ctrl_sock.select(Socket::SEL_WRITE, 2.0)<0)
    ERROR_FUNC("Waiting for connect\n");
  WARNING_FUNC("Got connect\n");
  END_FUNC("VRSocketThread");
}

void VRSocketThread::listen()
{
  START_FUNC("VRSocketThread");
  _ctrl_accept_sock.listen();
  END_FUNC("VRSocketThread");
}

void VRSocketThread::accept()
{
  START_FUNC("VRSocketThread");
  int sd=_ctrl_accept_sock.accept();
  if (sd<=0) {
    ERROR_FUNC("VRSocketThread") << "invalid accept\n";
    return;
  }
  _ctrl_sock=TCPSocket(sd, _mtu);
  END_FUNC("VRSocketThread");
}

int VRSocketThread::recv(SocketBuffer& s, const int& n)
{
  START_FUNC("VRSocketThread")
    if (_mode==PS_APP_BYE)
      return(-1);
  if (n<0) {
    throw SocketException("Zero size buffer");
  }
  SocketBufferCtrlBlk scb(_mtu);
  int ret=0;;
  _recv_mutex.lock();
  if (_recv_sockbufctrlblk_map.empty()) {
    _recv_mutex.unlock();
    DEBUG_FUNC("VRSocketThread") << "empty recv_sockbufctrlblk_list\n";
    return(0);
  } else {
    DEBUG_FUNC("VRSocketThread") << "non-empty recv_sockbufctrlblk_list\n";
    int key=_recv_sockbufctrlblk_map.begin()->first;
    s=_recv_sockbufctrlblk_map.begin()->second._sb;
    ret=_recv_sockbufctrlblk_map.begin()->second._bytes_read;
    _recv_sockbufctrlblk_map.erase(key);
  }	
  _recv_mutex.unlock();
  END_FUNC("VRSocketThread")
    return(ret);
}

int VRSocketThread::send(const SocketBuffer& s, const int& n)
{
  START_FUNC("VRSocketThread")
    if (_mode==PS_APP_BYE)
      return(-1);
  SocketBufferCtrlBlk scb(_mtu);
  scb._sb=s;
  // scb._bytes_read=s.size();
  scb._bytes_read=n;
  scb._bytes_sent=0;
  _send_mutex.lock();
  _send_sockbufctrlblk_list.push_back(scb);	
  _send_mutex.unlock();
  END_FUNC("VRSocketThread")
    return(n);
}

int VRSocketThread::shutdown(const int howto)
{
  START_FUNC("VRSocketThread")
    int ret=_data_sock.shutdown(howto);
  ret+=_ctrl_sock.shutdown(howto);
  ret+=_ctrl_accept_sock.shutdown(howto);
  END_FUNC("VRSocketThread")
    return(ret);
}

void VRSocketThread::close()
{
  START_FUNC("VRSocketThread")
    _data_sock.close();
  _ctrl_sock.close();
  _ctrl_accept_sock.close();
  END_FUNC("VRSocketThread")
    }

int VRSocketThread::get_send_buf_size()
{
  int ret=0;
  _send_mutex.lock();
  ret=_send_sockbufctrlblk_list.size();
  _send_mutex.unlock();
  return(ret);
}

int VRSocketThread::get_outstanding_buf_size() 
{
  int ret=0;
  _outstanding_mutex.lock();
  ret=_outstanding_sockbufctrlblk_map.size();
  _outstanding_mutex.unlock();
  return(ret);
}


ostream& operator<<(ostream& os, const VRSocketThread& s)
{
  os << "VRSocketThread dump\n";
  os << "-VRSocketThread dump\n";
  return(os);
}

#if 0
static bool parent()
{
  VRSocketThread s;
  string ip("0.0.0.0");
  int port(49000);
  s.bind(ip, port);
  int sd=s.accept();
  if (sd<=0)
    return(false);
  VRSocketThread c(sd, 1024);
  SocketBuffer sbuf;
  sbuf.reserve(1024);
  int rcvd_bytes=c.recv(sbuf, 1024);
  if (rcvd_bytes!=1024) {
    c.close();
    s.close();
    return(false);
  }
  for (int i=0; i<1024; i++) {
    if (sbuf[i]!=i%10) {
      c.close();
      s.close();
      return(false);
    }
  }
  c.close();
  s.close();
  return(true);
}

static bool child()
{
  string ip("127.0.0.1");
  int port(49000);
  VRSocketThread c;
  usleep(2000000);
  c.connect(ip, port);
  SocketBuffer sbuf(1024);
  for (int i=0; i<1024; i++) {
    sbuf[i]=i%10;
  }
  int sent_bytes=c.send(sbuf, 1024);
  if (sent_bytes!=1024) {
    c.close();
    return(false);
  }
  c.close();
  exit(0);
}

int VRSocketThread::get_send_buf_size() const
{
  return(_snd_sockbufctrlblk_list.size());
}
#endif
void VRSocketThread::print_pkt_hdr(const SocketBuffer& sb)
{
  START_FUNC("VRSocketThread")
    if (sb.size()<6)
      return;
  INFO_FUNC("VRSocketThread") << "Header==" 
                              << sb[0] << ":" << sb[1] << ":"
                              << sb[2] << ":" << sb[3] << ":"
                              << sb[4] << ":" << sb[5] << "\n";
  INFO_FUNC("VRSocketThread") << "Type==" 
                              << sb[1] << "\n";
  unsigned char hl=sb[2];
  unsigned char ll=sb[3];
  unsigned char hsn=sb[4];
  unsigned char lsn=sb[5];
  unsigned int length=(hl<<8)+ll;
  unsigned int sn=(hsn<<8)+lsn;
  INFO_FUNC("VRSocketThread") << "Length==" 
                              <<  length << "\n";
  INFO_FUNC("VRSocketThread") << "SN==" 
                              <<  sn << "\n";
  END_FUNC("VRSocketThread")
    }

int VRSocketThread::build_ack_hdr(SocketBuffer& sb, const int& sn, const int& sz)
{
  START_FUNC("VRSocketThread")
    const int hdr_size=5;
  if (sz<hdr_size)
    return(-1);
  sb[0]='a';		// "Synch" word.
  sb[1]=PT_ACK;	// Packet type.
  sb[2]=(0);		// Data length (==0 for ACK).
  sb[3]=(0);		// Data length (==0 for ACK).
  sb[4]=(sn>>8);	// Sequence number MSB.
  sb[5]=(sn);		// Sequence number LSB.
  return(0);
  END_FUNC("VRSocketThread")
    }

int VRSocketThread::build_data_hdr(SocketBuffer& sb, const int& sn, const int& sz,
                                   const int& length)
{
  START_FUNC("VRSocketThread")
    const int hdr_size=5;
  if (sz<hdr_size)
    return(-1);
  sb[0]='a';				// "Synch" word.
  sb[1]=PT_DATA;			// Packet type.
  sb[2]=(length>>8);		// Data length (==0 for ACK).
  sb[3]=(length);			// Data length (==0 for ACK).
  sb[4]=(sn>>8);			// Sequence number MSB.
  sb[5]=(sn);				// Sequence number LSB.
  return(0);
  END_FUNC("VRSocketThread")
    }

int VRSocketThread::build_fin_hdr(SocketBuffer& sb, const int& sz)
{
  START_FUNC("VRSocketThread")
    const int hdr_size=5;
  if (sz<hdr_size)
    return(-1);
  sb[0]='a';		// "Synch" word.
  sb[1]=PT_FIN;	// Packet type.
  sb[2]=(0);		// Data length (==0 for ACK).
  sb[3]=(0);		// Data length (==0 for ACK).
  sb[4]=(0);		// Sequence number MSB.
  sb[5]=(0);		// Sequence number LSB.
  return(0);
  END_FUNC("VRSocketThread")
    }

bool VRSocketThread::test()
{
  pid_t child_id=fork();
  bool ret=false;
  if (child_id==0)
    ; // ret=parent();
  else
    ; // ret=child();
  return(ret);
}
