#!/bin/bash

export LD_LIBRARY_PATH=/home/dlapsley/mark6/src/extern/PF_RING-4.7.0/userland/lib

./net2disk -i eth3 -n 16 -S 0,1,2,3,4,5,6,7,16,17,18,19,20,21,22,23
# ./net2disk -i eth3 -n 8 -S 0,1,2,3,4,5,6,7

