/*
 *  Object.h
 *  vrtp
 *
 *  Created by David Lapsley on Sat Feb 28 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <string>
#include <Logger.h>

#ifndef _OBJECT_H_
#define _OBJECT_H_

using namespace std;

class Object {
private:
public:
    Logger& debug_logger() const;
    Logger& info_logger() const;
    Logger& warning_logger() const;
    Logger& error_logger() const;
};

#endif // _OBJECT_H_

