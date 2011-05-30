/*
 *  RTPSession.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "RTPSession.h"

RTPSession::RTPSession()
{
    debug_logger() << "RTPSession::RTPSession()\n";
    debug_logger() << "-RTPSession::RTPSession()\n";
}

RTPSession::~RTPSession()
{
    debug_logger() << "RTPSession::~RTPSession()\n";
    debug_logger() << "-RTPSession::~RTPSession()\n";
}

void RTPSession::AddServer(Server& s)
{
    debug_logger() << "RTPSession::AddServer()\n";
    _server_list.push_back(s);
    debug_logger() << "-RTPSession::AddServer()\n";
}

void RTPSession::AddClient(Client& c)
{
    debug_logger() << "RTPSession::Addclient()\n";
    _client_list.push_back(c);
    debug_logger() << "-RTPSession::Addclient()\n";
}

void RTPSession::run(Options o)
{
    debug_logger() << "RTPSession::run()\n";

    // Launch servers.
    ServerVector::iterator s;
    for (s=_server_list.begin(); s!=_server_list.end(); s++) {
        s->run(o);
    }
    // Launch clients.
    ClientVector::iterator c;
    for (c=_client_list.begin(); c!=_client_list.end(); c++) {
        c->run(o);
    }
    debug_logger() << "-RTPSession::run()\n";
}

void RTPSession::join()
{
    debug_logger() << "RTPSession::join()\n";

    // Wait for servers.
    ServerVector::iterator s;
    for (s=_server_list.begin(); s!=_server_list.end(); s++) {
        s->join();
    }
    // Wait for clients.
    ClientVector::iterator c;
    for (c=_client_list.begin(); c!=_client_list.end(); c++) {
        c->join();
    }
    
    debug_logger() << "-RTPSession::join()\n";
}

