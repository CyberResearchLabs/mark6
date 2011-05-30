/*
 *  RTPSession.h
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Object.h>
#include <Options.h>
#include <Server.h>
#include <Client.h>
#include <vector>

#ifndef _RTPSESSION_H_
#define _RTPSESSION_H_

typedef vector<Client> ClientVector;
typedef vector<Server> ServerVector;

class RTPSession: public Object 
{
private:
    ClientVector _client_list;
    ServerVector _server_list;
public:
    RTPSession();
    ~RTPSession();
    void AddServer(Server& s);
    void AddClient(Client& c);
    void run(Options o);
    void join();
};

#endif // _RTPSESSION_H_

