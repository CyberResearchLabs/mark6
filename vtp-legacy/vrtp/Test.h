/*
 *  Test.h
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Options.h>
#include <Object.h>
#include <PSocket.h>

#ifndef _TEST_H_
#define _TEST_H_

class Test: public Object {
private:
public:
    Test();
    virtual ~Test();
    void run(Options o);
};

#endif // _TEST_H_

