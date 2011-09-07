#!/bin/bash

export LD_LIBRARY_PATH=/usr/local/lib

EXEC=dim6test
NICE_LEVEL=-5

nice ${NICE_LEVEL} ./${EXEC} 
# ionice -c1 -p $PPID

