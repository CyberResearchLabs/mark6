/*
 *  Utils.h
 *  vrtp
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Carbon/Carbon.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <string.h>

class InternetAddress {
private:
    struct sockaddr _addr;
    socklen_t _addrlen;
public:
        InternetAddress() {
            memset(&_addr, '\0', sizeof(_addr));
            memset(&_addrlen, '\0', sizeof(_addrlen));
        }
    InternetAddress(struct sockaddr addr) {
        memset(&_addr, '\0', sizeof(_addr));
        _addrlen=sizeof(_addr);
    }
};