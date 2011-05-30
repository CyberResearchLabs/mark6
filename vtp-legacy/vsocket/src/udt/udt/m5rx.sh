#!/bin/bash

# nc -vv -l -p 2630 127.0.0.1 
nc -vv -l -p 2630 127.0.0.1 > output.dat &
nc -vv -l -p 2620 127.0.0.1 > ctrl.dat
