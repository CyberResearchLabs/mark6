#!/bin/bash

../../bin/vsocket.app -c -I 127.0.0.1 -P 49000 -F input.dat -m 1024 -M 1024 -n 8 -R 100 -B 1000000000 -D 0

# md5sum input.dat >> ../server/sum.out
