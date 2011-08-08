#include <unistd.h>

#include <string>
#include <iostream>

#include <boost/foreach.hpp>
#include <buffer_pool.h>

BufferPool* BufferPool::_inst = NULL;
