#!/bin/bash

# ./vrtp -s -i 127.0.0.1 -p 49000 -F input.dat -D 2 -n 8 -r 0

# ./appserver -i 0.0.0.0 -p 2630 -I 192.52.61.179 -P 49000 -m 384000 -M 768000 -S 1000 -r 1 &
# ./appserver -i 0.0.0.0 -p 2630 -I 192.52.61.179 -P 49000 -m 1000000 -M 5000000 -S 1000 -r 3 >& server.out &
./appserver -i 0.0.0.0 -p 2630 -I 192.52.61.179 -P 49000 -m 1000 -M 5000000 -S 1000 -r 4 -R 40000000
# cat input.dat | nc -vv 127.0.0.1 2630
