#!/bin/bash

echo Hello $1 > /tmp/exec.txt
echo b3ack
/usr/bin/nc -l -p 49011 >> /tmp/exec.txt
