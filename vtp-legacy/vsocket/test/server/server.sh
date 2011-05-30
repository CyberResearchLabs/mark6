#!/bin/bash

../../bin/vsocket.app -s -i 0.0.0.0 -p 49000 -T output.dat -m 1024 -M 1024 -n 8 -R 100 -B 1000000000 -D 0

md5sum output.dat >> sum.out
tail sum.out
