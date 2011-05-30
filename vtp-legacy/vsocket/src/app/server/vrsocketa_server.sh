#!/bin/bash

./vrsocketa_test -s -i 0.0.0.0 -p 49000 -T output.dat -m 1024 -M 1024 -n 1 -D 0

md5sum output.dat >> md5.out
tail md5.out
