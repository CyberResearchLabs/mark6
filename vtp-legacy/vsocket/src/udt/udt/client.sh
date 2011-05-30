#!/bin/bash 

# ./vrtp -c -I 0.0.0.0 -P 49000 -D 2 -r 0 -T out.dat -n 8
# ./vrtp -c -i 0.0.0.0 -p 49000 -I 140.173.180.6 -P 2630 -D 3 -r 1 -T output.dat -n 8

/home/dlapsley/bin/nc -vv -l -p 2631 127.0.0.1 > output.dat &

sleep 5

./appclient -i 0.0.0.0 -p 49000 -I 127.0.0.1 -P 2631 -m 3072000 -S 1000 -r 1


