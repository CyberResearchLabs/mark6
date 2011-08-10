#!/bin/bash

export LD_LIBRARY_PATH=/home/dlapsley/mark6/src/extern/PF_RING-4.7.0/userland/lib

# ./net2disk --interfaces eth2 --capture_files /mnt/disk0/1.m6 
# ./net2disk --interfaces eth2 eth3 --capture_files /mnt/disk0/1.m6 /mnt/disk1/1.m6 

# ./net2disk --interfaces eth2 eth3 eth4 eth5 --capture_files /mnt/disk0/1.m6 /mnt/disk1/1.m6 /mnt/disk2/1.m6 /mnt/disk3/1.m6

./net2disk --interfaces eth2 --capture_files /mnt/disk0/1.m6 
