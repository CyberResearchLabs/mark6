/*
 *  PSocket.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Fri Mar 19 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <sys/socket.h>
#include <iostream>
#include <exception>
#include "PSocket.h"

PSocket::PSocket(int num_ports, int mtu)
{
    debug_logger() << "PSocket::PSocket()\n";
    _num_ports=num_ports;
    _mtu=mtu;
    int i;
    for (i=0; i<_num_ports; ++i) {
        TCPSocket s;
        _sock_list.push_back(s);
    }
    debug_logger() << "-PSocket::PSocket()\n";    
}

PSocket::PSocket(const PSocket &s)
{
    debug_logger() << "PSocket::PSocket()\n";
    debug_logger() << "-PSocket::PSocket()\n";
}

PSocket::~PSocket()
{
    debug_logger() << "PSocket::~PSocket()\n";
    debug_logger() << "-PSocket::~PSocket()\n";
}

PSocket& PSocket::operator=(const PSocket& s)
{
    debug_logger() << "PSocket::operator=()\n";
    return(*this);
    debug_logger() << "-PSocket::operator=()\n";    
}

const int PSocket::get_mtu() const
{
    debug_logger() << "PSocket::get_mtu()\n";
    return(_mtu);
    debug_logger() << "-PSocket::get_mtu()\n";    
}

void PSocket::bind(string ip, int port)
{
    debug_logger() << "PSocket::bind()\n";
    try {
        for (_sock_list_iter=_sock_list.begin(); _sock_list_iter!=_sock_list.end(); 
             ++_sock_list_iter)
            _sock_list_iter->bind(ip, port++);        
    } catch (...) {
        debug_logger() << "    Error in bind()\n";
    }
    debug_logger() << "-PSocket::bind()\n";
}

void PSocket::listen()
{
    debug_logger() << "PSocket::listen()\n";
    try {
        for (_sock_list_iter=_sock_list.begin(); _sock_list_iter!=_sock_list.end(); 
             ++_sock_list_iter)
            _sock_list_iter->listen();
    } catch (...) {
        debug_logger() << "    Error in listen()\n";        
    }
    debug_logger() << "-PSocket::listen()\n";    
}

void PSocket::accept()
{
    debug_logger() << "PSocket::accept()\n";
    try {
        for (_sock_list_iter=_sock_list.begin(); _sock_list_iter!=_sock_list.end(); 
             ++_sock_list_iter) {
            int ret=_sock_list_iter->accept();
            TCPSocket s(ret);
            _accept_sock_list.push_back(s);
            for (_accept_sock_list_iter=_accept_sock_list.begin();
                 _accept_sock_list_iter!=_accept_sock_list.end();
                 ++_accept_sock_list_iter) {
                int sockd=_accept_sock_list_iter->get_sockd();
                debug_logger() << "    accept sock list\n";
                debug_logger() << "         _sockd=" << sockd  << "\n";
            }
        }
        // Important! Have to set back to the begginning!
        _accept_sock_list_iter=_accept_sock_list.begin();
    } catch (...) {
        debug_logger() << "    Error in accept()\n";
    }
    debug_logger() << "-PSocket::accept()\n";        
}

void PSocket::connect(string ip, int port)
{
    debug_logger() << "PSocket::connect()\n";
    try {
        for (_sock_list_iter=_sock_list.begin(); 
             _sock_list_iter!=_sock_list.end(); ++_sock_list_iter)
            _sock_list_iter->connect(ip, port++);
        // Important! Have to set back to the begginning!
        _sock_list_iter=_sock_list.begin();
    } catch (...) {
        debug_logger() << "    Error in connect to: " << ip << ":" << port
        << "\n";
    }
    debug_logger() << "-PSocket::connect()\n";
}

int PSocket::recv(SocketBuffer& s)
{
    int bytes_rcvd; // Includes 2B overhead.
    int data_rcvd;  // Pure data.
    int length;
    SocketBuffer _sbuf(_mtu+2);
    char* sbuf=(char*)_sbuf;
    char* sp=(char*)s;
    debug_logger() << "PSocket::recv()\n";
    try {
        // Set the socket buffer's request size to be the sizeof the 
        // "s" socket buffer's request + the size of the length header.
#if 1
        _sbuf.clear();
        _sbuf.set_req_size(s.get_req_size()+2);
        bytes_rcvd=_accept_sock_list_iter->recv(_sbuf);
        data_rcvd=bytes_rcvd-2;
        if (bytes_rcvd==-1)
            throw PSocketError("early recv() return");
        if (bytes_rcvd==0) {
            s.clear();
            return(0);
        }
        if (bytes_rcvd<2)
            throw PSocketError("short message recv()'d");
        // Extract length field.
        length=(sbuf[0]<<8)+sbuf[1];
        int bytes_left=length-data_rcvd;
        // Store first part of data in buffer.
        memcpy(sp, sbuf+2, data_rcvd);
        sp+=data_rcvd;
        s.set_size(data_rcvd);
        while (bytes_left>0) {
            info_logger() << "    bytes left==" << bytes_left << "\n";
            _sbuf.clear();
            _sbuf.set_req_size(bytes_left);
            bytes_rcvd=_accept_sock_list_iter->recv(_sbuf);
            data_rcvd+=bytes_rcvd;
            if (bytes_rcvd==-1)
                throw PSocketError("early recv() return");
            if (bytes_rcvd==0) {
                warning_logger() << "    0 bytes received\n";
            }
            strncpy(sp, sbuf, bytes_rcvd);
            sp+=bytes_rcvd;
            bytes_left-=bytes_rcvd;
        }
#else
        data_rcvd=_accept_sock_list_iter->recv(s);
        if (data_rcvd<_mtu)
            warning_logger() << "Bugger\n";
#endif
        ++_accept_sock_list_iter;
        if (_accept_sock_list_iter==_accept_sock_list.end())
            _accept_sock_list_iter=_accept_sock_list.begin();
    } catch (exception e) {
        debug_logger() << "    Error in PSocket recv(): " << e.what() << "\n";
    }
    debug_logger() << "-PSocket::recv()\n";
    return(data_rcvd);
}

int PSocket::recv(string& s, int n)
{
    int bytes_rcvd; // Includes 2B overhead.
    int data_rcvd;  // Pure data.
    int length;
    static int tbr=0;
    debug_logger() << "PSocket::recv()\n";
    try {
        do {
            data_rcvd=bytes_rcvd=_accept_sock_list_iter->recv(_buf, n);
        } while (data_rcvd<0);
        tbr+=bytes_rcvd;
        s=_buf;
        int bytes_left=n-data_rcvd;
        while (bytes_left>0) {
            warning_logger() << " short packet. bytes_left: " << bytes_left << "\n";
            bytes_rcvd=_accept_sock_list_iter->recv(_buf, bytes_left);
            if (bytes_rcvd<0)
                continue;
            tbr+=bytes_rcvd;
            if (bytes_rcvd==0) {
                warning_logger() << "    zero bytes received!\n";
                break;
            }
            warning_logger() << " bytes_rcvd: " << bytes_rcvd << "\n";
            warning_logger() << " data_rcvd: " << bytes_rcvd << "\n";
            s.append(_buf, 0, bytes_rcvd);
            data_rcvd+=bytes_rcvd;
            bytes_left-=bytes_rcvd;
        }
        warning_logger() << " psocket::tbr==" << tbr << "\n";
        ++_accept_sock_list_iter;
        if (_accept_sock_list_iter==_accept_sock_list.end())
            _accept_sock_list_iter=_accept_sock_list.begin();
    } catch (exception e) {
        debug_logger() << "    Error in PSocket recv(): " << e.what() << "\n";
    }
    debug_logger() << "-PSocket::recv()\n";
    return(data_rcvd);
}


int PSocket::send(const SocketBuffer& s)
{
    debug_logger() << "PSocket::send()\n";
    int bytes_sent; // Includes data overhead.
    int data_sent;  // Pure data sent.
    u_int16_t length;
    SocketBuffer _sbuf(_mtu+2);
    char* sbuf=(char*)_sbuf;
    char* sp=(char*)s;

    try {
#if 1
        length=s.get_req_size();
        sbuf[0]=(length>>8) & 0xff;
        sbuf[1]=length & 0xff;
        _sbuf.set_size(2);
        if (s.get_max_size()>_mtu)
            debug_logger() << "    send: input buffer too large\n";
        sbuf+=2;
        memcpy(sbuf, s.get_buf(), s.get_req_size());
        _sbuf.set_size(2+s.get_req_size());
        _sbuf.set_req_size(_sbuf.get_size());
        int bytes_left=length;
        bytes_sent=_sock_list_iter->send(_sbuf);
        data_sent=bytes_sent-2;
        if (bytes_sent==-1)
            throw PSocketError("early sent() return");
        else if (bytes_sent==0) {
            warning_logger() << "    0 bytes sent\n";
            throw PSocketError("0 bytes sent\n");
        }
        bytes_left-=(bytes_sent-2);
        if (bytes_left>0)
            throw PSocketError("    short send() error");
        _sbuf.clear();
#else
        data_sent=_sock_list_iter->send(s);
        if (data_sent<_mtu)
            warning_logger() << "Bugger\n";
#endif
        ++_sock_list_iter;
        if (_sock_list_iter==_sock_list.end())
            _sock_list_iter=_sock_list.begin();
        debug_logger() << "    Sent: " << bytes_sent << " bytes\n";
    } catch (SocketException e) {
        error_logger() << "    Error in Recv(): " << e.what() << "\n";
    }
    debug_logger() << "-PSocket::send()\n";
    return(data_sent);
}

int PSocket::send(const string& s, const int n)
{
    debug_logger() << "PSocket::send()\n";
    int bytes_sent; // Includes data overhead.
    int data_sent;  // Pure data sent.
    static int tbs=0;
    try {
        do {
            data_sent=_sock_list_iter->send(s, n);
        } while (data_sent<0);
        tbs+=data_sent;
        warning_logger() << "   psocket tbs == " << tbs << "\n";
        if (data_sent<n) {
            warning_logger() << "Send Bugger\n";        
            int bytes_left=n-data_sent;
            int total_data_sent=data_sent;
            while (bytes_left>0) {
                string ss;
                ss.assign(s, total_data_sent, bytes_left);
                data_sent=_sock_list_iter->send(ss, bytes_left);
                if (data_sent<0)
                    continue;
                total_data_sent+=data_sent;
                bytes_sent-=data_sent;
            }
        }
        ++_sock_list_iter;
        if (_sock_list_iter==_sock_list.end())
            _sock_list_iter=_sock_list.begin();
        debug_logger() << "    Sent: " << data_sent << " bytes\n";
    } catch (exception e) {
        error_logger() << "    Error in send(): " << e.what() << "\n";
    }
    debug_logger() << "-PSocket::send()\n";
    return(data_sent);
}

void PSocket::close()
{
    debug_logger() << "PSocket::close()\n";
    // XXXX Under construction.
    for (_sock_list_iter=_sock_list.begin(); _sock_list_iter!=_sock_list.end();
         ++_sock_list_iter)
        _sock_list_iter->close();
    for (_accept_sock_list_iter=_accept_sock_list.begin(); 
         _accept_sock_list_iter!=_accept_sock_list.end();
         ++_accept_sock_list_iter)
        _accept_sock_list_iter->close();
    debug_logger() << "-PSocket::close()\n";    
}

int PSocket::shutdown(int howto)
{
    int ret=0;
    debug_logger() << "PSocket::shutdown()\n";
    // XXXX Under construction.
    for (_sock_list_iter=_sock_list.begin(); _sock_list_iter!=_sock_list.end();
         ++_sock_list_iter)
        ret+=_sock_list_iter->shutdown(howto);
    for (_accept_sock_list_iter=_accept_sock_list.begin(); 
         _accept_sock_list_iter!=_accept_sock_list.end();
         ++_accept_sock_list_iter)
        ret+=_accept_sock_list_iter->shutdown(howto);
    if (ret!=0)
        error_logger() << "    Unable to shutdown all TCP connections\n";
    debug_logger() << "-PSocket::shutdown()\n";    
    return(ret);
}


