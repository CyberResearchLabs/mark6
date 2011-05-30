/*
 *  MyException.h
 *  vrtp
 *
 *  Created by David Lapsley on Fri Feb 20 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <string>

using namespace std;

class MyException {
private:
    int _n;
    string _message;
public:
    MyException(int n) {
        _n=n;
    }
    MyException(int n, string s) {
        _n=n;
        _message=s;
    }
    ~MyException()
    {
    }
    const char* what() {
        const char* s=(char*)_message.data();
        return (s);
    }
};

