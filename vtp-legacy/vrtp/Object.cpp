/*
 *  Object.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sat Feb 28 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */
#include <iostream>
#include <Logsystem.h>

#include "Object.h"

Logger& Object::debug_logger() const
{
    return(*Logsystem::debug_logger());
}  // End cerr.

Logger& Object::info_logger() const
{
    return(*Logsystem::info_logger());
}  // End info_logger().

Logger& Object::warning_logger() const
{
    return(*Logsystem::warning_logger());
}  // End warning_logger().

Logger& Object::error_logger() const
{
    return(*Logsystem::error_logger());    
}  // End error_logger().

