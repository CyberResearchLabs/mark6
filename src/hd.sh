#!/bin/bash

WRITE_MBYTES=100000
STATS_FILE=hd_stats.dat

/bin/rm -f ${STATS_FILE} 

# for BLOCK_SIZE in 1024 2048 4096 8192 16384 32768 65536 131072 262144 524288
# for BLOCK_SIZE in 1024 2048 3072 4096 5120 8192 
for BLOCK_SIZE in 4096 8192 
do
	echo BLOCK_SIZE ${BLOCK_SIZE} WRITE_MBYTES ${WRITE_MBYTES}
	sudo ./mark6 \
		--test-hd \
		--write-mbytes ${WRITE_MBYTES} \
		--write-block-size ${BLOCK_SIZE} | tee -a ${STATS_FILE}
done

sudo find /mnt -name *.m6 -exec rm -f {} \;

grep STATS ${STATS_FILE}
