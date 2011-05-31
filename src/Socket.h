/*
 *  Socket.h
 *  vrtp
 *
 *  Created by David Lapsley on Fri Feb 20 2004.
 *
 * Copyright 2004, 2011 MIT Haystack Observatory 
 *  
 * This file is part of mark6.
 *
 * mark6 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * mark6 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with mark6.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#include <string>
#include <SocketBuffer.h>
#ifdef LINUX
#include <unistd.h>
#endif // LINUX
#include <sys/socket.h>

#ifndef _SOCKET_H_
#define _SOCKET_H_

using namespace std;

namespace vtp {
  class SocketException {
  private:
    int _n;
    string _message;
  public:
    SocketException(int n) {
      _n=n;
    }
    SocketException(int n, string s) {
      _n=n;
      _message=s;
    }
    ~SocketException()
      {
      }
    const char* what() {
      const char* s=(char*)_message.data();
      return (s);
    }
  };

  // Stream socket.
  class Socket {
  protected:
    int _sockd;
  public:
    Socket() {
      _sockd=-1;
    }
    virtual void close() = 0;
#ifdef _DAVE_DEBUG
    virtual int test(void*);
    virtual int dump(void*);
#endif
  };

}

#endif
