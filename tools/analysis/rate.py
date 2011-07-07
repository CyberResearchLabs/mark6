#!/usr/bin/python

import os, sys
from stat import *

bytes_written = 0
number_of_disks = 7
mount_prefix = '/mnt/disk'
for i in range(number_of_disks):
	file_name = '%s%d/test.m6'%(mount_prefix, i)
	size = os.stat(file_name)[ST_SIZE]
	print size
	bytes_written += size


time=30
rate=8e9
throughput=bytes_written*8/(time*1000000)

print time, ' s'
print rate, ' bps'
print bytes_written/1e6, ' MB'
print throughput, ' Mbps'
