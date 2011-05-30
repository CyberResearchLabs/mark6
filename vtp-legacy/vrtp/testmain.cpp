/*
 *  testmain.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sun Mar 07 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Logger.h>
#include <ErrorLogger.h>
#include <InfoLogger.h>


int main(int argc, char** argv)
{
    InfoLogger* my_info_logger=InfoLogger::Instance();
    *my_info_logger << "Hello world from Info logger\n" << 3.14 << "\n";
    
    ErrorLogger* my_error_logger=ErrorLogger::Instance();
    *my_error_logger << "Hello world from Error logger\n" << 1592654 << "\n";    
}
