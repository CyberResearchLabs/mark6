/*
 * Created by David Lapsley on Mon Jun 6 2011.
 *
 * Copyright 2011 MIT Haystack Observatory 
 *  
 * This file is part of mark6.
 *
 * mark6 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the Li!cense, or
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

#ifndef _THREADED_H_
#define _THREADED_H_

// C includes.

// C++ includes

// Framework includes.
#include <boost/thread/thread.hpp>

// Local includes
#include <mark6.h>

class Threaded
{
 public:
  virtual void start() = 0;
  virtual void join() = 0;
  virtual void cmd_stop() = 0;
 protected:
  virtual void run() = 0;
};

#endif // _THREADED_H_
