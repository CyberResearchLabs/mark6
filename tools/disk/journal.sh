#!/bin/bash

# Author:	del@haystack.mit.edu
# Description:	Disable journaling.


DEVS=$(cat <<EOF
/dev/sdb
/dev/sdc
/dev/sdd
/dev/sde
/dev/sdf
/dev/sdg
/dev/sdh
EOF
)

# /dev/sdaa

DEV_MAP[1]="/dev/sdb:/mnt/disk0"
DEV_MAP[2]="/dev/sdc:/mnt/disk1"
DEV_MAP[3]="/dev/sdd:/mnt/disk2"
DEV_MAP[4]="/dev/sde:/mnt/disk3"
DEV_MAP[5]="/dev/sdf:/mnt/disk4"
DEV_MAP[6]="/dev/sdg:/mnt/disk5"
DEV_MAP[7]="/dev/sdh:/mnt/disk6"

TUNE2FS=/sbin/tune2fs
E2FSCK=/sbin/e2fsck
DUMPE2FS=/sbin/dumpe2fs
MOUNT=/bin/mount


config_devs() {
	for d in ${DEVS}
	do
		echo Configuring ${d}
		# Enable writeback mode. This mode will typically provide the best ext4 performance.
		${TUNE2FS} -o journal_data_writeback ${d}

		# Delete has_journal option
		${TUNE2FS} -O ^has_journal  ${d}
	
		# Required fsck
		${E2FSCK} -f ${d}

		# Check fs options
		${DUMPE2FS} ${d}
	done
}

mount_devs() {
	MOUNT_OPTS=defaults,data=writeback,noatime,nodiratime
	for p in ${DEV_MAP[@]}
	do
		IFS=':' read -ra a <<< "$p"
		dev=${a[0]}
		mnt=${a[1]}

		echo ${MOUNT} -t ext4 -o ${MOUNT_OPTS} ${dev} ${mnt}
		${MOUNT} -t ext4 -o ${MOUNT_OPTS} ${dev} ${mnt}
	done
}

# config_devs
mount_devs
