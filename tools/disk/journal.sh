#!/bin/bash

# Author:	del@haystack.mit.edu
# Description:	Disable journaling.


DEVS=$(cat <<EOF
/dev/sdb1
/dev/sdc1
/dev/sdd1
/dev/sde1
/dev/sdf1
/dev/sdg1
/dev/sdh1
/dev/sdi1
/dev/sdj1
/dev/sdk1
/dev/sdl1
/dev/sdm1
/dev/sdn1
/dev/sdo1
/dev/sdp1
/dev/sdq1
EOF
)

# /dev/sdaa

DEV_MAP[1]="/dev/sdb1:/mnt/disk0"
DEV_MAP[2]="/dev/sdc1:/mnt/disk1"
DEV_MAP[3]="/dev/sdd1:/mnt/disk2"
DEV_MAP[4]="/dev/sde1:/mnt/disk3"
DEV_MAP[5]="/dev/sdf1:/mnt/disk4"
DEV_MAP[6]="/dev/sdg1:/mnt/disk5"
DEV_MAP[7]="/dev/sdh1:/mnt/disk6"
DEV_MAP[8]="/dev/sdi1:/mnt/disk7"
DEV_MAP[9]="/dev/sdj1:/mnt/disk8"
DEV_MAP[10]="/dev/sdk1:/mnt/disk9"
DEV_MAP[11]="/dev/sdl1:/mnt/disk10"
DEV_MAP[12]="/dev/sdm1:/mnt/disk11"
DEV_MAP[13]="/dev/sdn1:/mnt/disk12"
DEV_MAP[14]="/dev/sdo1:/mnt/disk13"
DEV_MAP[15]="/dev/sdp1:/mnt/disk14"
DEV_MAP[16]="/dev/sdq1:/mnt/disk15"

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
		mkdir -p ${mnt}
		${MOUNT} -t ext4 -o ${MOUNT_OPTS} ${dev} ${mnt}
	done
}

config_devs
mount_devs
