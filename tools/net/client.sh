#!/bin/bash

IPERF=/usr/bin/iperf
PING=/bin/ping

DEST_IP=10.0.0.1
PORT=4242
WRITE_BUFFER_SIZE=32K
BANDWIDTH=4G
THREADS=1
TIME=30
INTERVAL=1

${PING} -c 1 ${DEST_IP}

echo ${IPERF} -c ${DEST_IP} -u -p ${PORT}
${IPERF} -c ${DEST_IP} -u \
	-p ${PORT} \
	-l ${WRITE_BUFFER_SIZE} \
	-b ${BANDWIDTH}  \
	-P ${THREADS} \
	-t ${TIME} \
	-i ${INTERVAL}
	# -B ${INTERFACE}
