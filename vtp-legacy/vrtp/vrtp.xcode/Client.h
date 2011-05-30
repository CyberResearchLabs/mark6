/*
 *  Client.h
 *  vrtp
 *
 *  Created by David Lapsley on Wed Feb 25 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Carbon/Carbon.h>
#include <Options.h>

#ifndef _CLIENT_H_
#define _CLIENT_H_

class Client {
private:
public:
    Client();
    ~Client();
    void run(Options o);
    void test(Options o);
};

#endif // _CLIENT_H_