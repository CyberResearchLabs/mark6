#!/bin/bash

# Author:	del@haystack.mit.edu
# Description:	Disable journaling.


DEVS=$(cat <<EOF
/dev/sd11
/dev/sdb1
/dev/sdc1
/dev/sdd1
EOF
)
# /dev/sdaa


# Devices
DEV_MAP[1]="/dev/sda1:/mnt/disk0"
DEV_MAP[2]="/dev/sdb1:/mnt/disk1"
DEV_MAP[3]="/dev/sdc1:/mnt/disk2"
DEV_MAP[4]="/dev/sdd1:/mnt/disk3"

# Executables
TUNE2FS=/sbin/tune2fs
E2FSCK=/sbin/e2fsck
DUMPE2FS=/sbin/dumpe2fs
MOUNT=/bin/mount
MEGACLI=/usr/sbin/megacli
PARTED=/sbin/parted
MKFS=/sbin/mkfs.ext4


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

mk_raid() {
	${MEGACLI}  -CfgClr -a1
	${MEGACLI} -CfgLdAdd -R0[245:8,245:9,245:10,245:11,245:12,245:13,245:14,245:15] WT NORA -strpsz 256 -a1
	${MEGACLI} -CfgLdAdd -R0[245:0,245:1,245:2,245:3,245:4,245:5,245:6,245:7] WT NORA -strpsz 256 -a1
	# ${MEGACLI} -CfgLdAdd -R0[245:8,245:9,245:10,245:11,245:12,245:13,245:14,245:15] WT NORA -a1
	# ${MEGACLI} -CfgLdAdd -R0[245:0,245:1,245:2,245:3,245:4,245:5,245:6,245:7] WT NORA -a1
}

mk_fs() {
	echo ${PARTED} -s /dev/sdd mklabel gpt
	${PARTED} -s /dev/sdd mklabel gpt

	echo ${PARTED} -s -- /dev/sdd mkpart primary ext4 1 -1
	${PARTED} -s -- /dev/sdd mkpart primary ext4 1 -1

	echo ${MKFS} /dev/sdd1
	${MKFS} /dev/sdd1
}

# mk_raid
# mk_fs
config_devs
mount_devs
