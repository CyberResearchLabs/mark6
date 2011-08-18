#!/bin/bash

# Author:	del@haystack.mit.edu
# Description:	Disable journaling.


DEVS=$(cat <<EOF
sda1
sdb1
sdc1
sdd1
sde1
sdf1
sdg1
sdh1
sdi1
sdj1
sdk1
sdl1
sdm1
sdn1
sdo1
sdp1
sdq1
sdr1
sds1
sdt1
sdu1
sdv1
sdw1
sdx1
sdy1
sdz1
sdaa1
sdab1
sdac1
sdad1
sdae1
sdaf1
sdag1
sdah1
sdai1
sdaj1
sdak1
sdal1
sdam1
sdan1
EOF
)
# /dev/sdaa


# Devices
# DEV_MAP[1]="/dev/sda1:/mnt/disk0"
# DEV_MAP[1]="/dev/sda1:/mnt/disk0"
# DEV_MAP[2]="/dev/sdb1:/mnt/disk1"
# DEV_MAP[3]="/dev/sdc1:/mnt/disk2"
# DEV_MAP[4]="/dev/sdd1:/mnt/disk3"

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
	${MEGACLI}  -CfgClr -aALL
	# ${MEGACLI} -CfgLdAdd -R0[245:0,245:1,245:2,245:3,245:4,245:5,245:6,245:7,245:8,245:9,245:10,245:11] WT NORA -strpsz 256 -a0
	# ${MEGACLI} -CfgLdAdd -R0[245:12,245:13,245:14,245:15,245:16,245:17,245:18,245:19,245:20,245:21,245:22,245:23] WT NORA -strpsz 256 -a0
	# ${MEGACLI} -CfgLdAdd -R0[245:0,245:1,245:2,245:3,245:4,245:5,245:6,245:7,245:8,245:9,245:10,245:11] WT NORA -strpsz 256 -a1
	# ${MEGACLI} -CfgLdAdd -R0[245:12,245:13,245:14,245:15,245:16,245:17,245:18,245:19,245:20,245:21,245:22,245:23] WT NORA -strpsz 256 -a1

	# Individual disk testing
  	# ${MEGACLI} -CfgForeign -Clear -aALL
  	# ${MEGACLI} -CfgEachDiskRaid0 -aALL
}

mk_part() {
	for DEV in ${DEV_MAP[@]}
	do
		IFS=':' read -ra a <<< "$DEV"
		dev=${a[0]}
		mnt=${a[1]}

		echo ${PARTED} /dev/${dev} --script mklabel gpt
		${PARTED} /dev/${dev} --script mklabel gpt
	
		echo ${PARTED} /dev/${dev} --script rm 1
		${PARTED} /dev/${dev} --script rm 1

		echo ${PARTED} /dev/${dev} --script mkpart ext4 1049k -- -1
		${PARTED} /dev/${dev} --script mkpart ext4 1049k -- -1
	done
}

mk_fs() {
	for DEV in ${DEV_MAP[@]}
	do
		IFS=':' read -ra a <<< "$DEV"
		dev=${a[0]}
		mnt=${a[1]}

		echo ${MKFS} /dev/${dev}1
		${MKFS} /dev/${dev}1
	done
}

# mk_raid
# mk_part
# mk_fs
# config_devs
# mount_devs

i=1
for d in ${DEVS}
do
	echo /dev/${d} /mnt/disk${i}
	DEV_MAP[${i}]="/dev/${d}:/mnt/disk${i}"
	let "i=i+1"
done

for f in ${DEV_MAP[@]}
do
	echo $f
done
