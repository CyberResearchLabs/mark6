#!/bin/bash

ps aux | grep run | awk '{print $2; }' | xargs sudo kill -9 
ps aux | grep net2disk | awk '{print $2; }' | xargs sudo kill -9 

