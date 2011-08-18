#!/bin/bash

FIO=/usr/bin/fio
CONFIG=jobs.fio
OUTPUT=jobs.out

${FIO}	--output=${OUTPUT} \
	--minimal \
	${CONFIG}

# jobname, groupid, error, 
# Read status:
# KB I/O, bandwidth (KB/s), runtime (ms)

# Submission latency:
# min, max, mean, standard deviation

# Completion latency:
# min, max, mean, standard deviation
# 
# Bandwidth:
# min, max, aggregate percentage of total, mean, standard deviation

# Write status:
# KB I/O, bandwidth (KB/s), runtime (ms)

# Submission latency:
# min, max, mean, standard deviation
# Completion latency:
# min, max, mean, standard deviation
# Bandwidth:
# min, max, aggregate percentage of total, mean, standard deviation

# CPU usage:
# user, system, context switches, major page faults, minor page faults

# IO depth distribution:
# <=1, 2, 4, 8, 16, 32, >=64
# 
# IO latency distribution (ms):
# <=2, 4, 10, 20, 50, 100, 250, 500, 750, 1000, >=2000
# 
# text description
