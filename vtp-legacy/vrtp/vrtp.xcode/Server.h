/*
 *  Server.h
 *  vrtp
 *
 *  Created by David Lapsley on Wed Feb 25 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Options.h>

#ifndef _SERVER_H_
#define _SERVER_H_

class Server {
private:
public:
    Server();
    ~Server();
    void run(Options o);
    void test(Options o);
};

#endif // _SERVER_H_