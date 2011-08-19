#!/bin/bash

# Author:	del@haystack.mit.edu
# Description:	Disable journaling.


DEVS=$(cat <<EOF
sdb
sdc
sdd
sde
sdf
sdg
sdh
sdi
EOF
)
# sda
# sdb
# sdc
# sdd
# sde
# sdf
# sdg
# sdh
# sdi
# sdj
# sdk
# sdl
# sdm
# sdn
# sdo
# sdp
# sdq
# sdr
# sds
# sdt
# sdu
# sdv
# sdw
# sdx
# sdy
# sdz
# sdaa
# sdab
# sdac
# sdad
# sdae
# sdaf
# sdag
# sdah
# sdai
# sdaj
# sdak
# sdal
# sdam
# sdan

init_dev_map() {
	echo "Devices to be configured"
	i=0
	for d in ${DEVS}
	do
		echo /dev/${d} /mnt/disk${i}
		DEV_MAP[${i}]="/dev/${d}:/mnt/disk${i}"
		let "i=i+1"
	done
}

# Executables
TUNE2FS=/sbin/tune2fs
E2FSCK=/sbin/e2fsck
DUMPE2FS=/sbin/dumpe2fs
MOUNT=/bin/mount
MEGACLI=/usr/sbin/megacli
PARTED=/sbin/parted
MKFS=/sbin/mkfs.ext4
FIO=/usr/bin/fio
CONFIG=jobs.fio
OUTPUT=jobs.out


tune_devs() {
	for d in ${DEVS}
	do
		echo Configuring /dev/${d}1
		# Enable writeback mode. This mode will typically provide the best ext4 performance.
		${TUNE2FS} -o journal_data_writeback /dev/${d}1

		# Delete has_journal option
		${TUNE2FS} -O ^has_journal  /dev/${d}1
	
		# Required fsck
		${E2FSCK} -f /dev/${d}1

		# Check fs options
		${DUMPE2FS} /dev/${d}1
	done
}

mount_devs() {
	MOUNT_OPTS=defaults,data=writeback,noatime,nodiratime
	for p in ${DEV_MAP[@]}
	do
		IFS=':' read -ra a <<< "$p"
		dev=${a[0]}
		mnt=${a[1]}

		echo ${MOUNT} -t ext4 -o ${MOUNT_OPTS} ${dev}1 ${mnt}
		mkdir -p ${mnt}
		${MOUNT} -t ext4 -o ${MOUNT_OPTS} ${dev}1 ${mnt}
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

		echo ${PARTED} ${dev} --script mklabel gpt
		${PARTED} ${dev} --script mklabel gpt
	
		echo ${PARTED} ${dev} --script rm 1
		${PARTED} ${dev} --script rm 1

		echo ${PARTED} ${dev} --script mkpart ext4 1049k -- -1
		${PARTED} ${dev} --script mkpart ext4 1049k -- -1
	done
}

mk_fs() {
	for DEV in ${DEV_MAP[@]}
	do
		IFS=':' read -ra a <<< "$DEV"
		dev=${a[0]}
		mnt=${a[1]}

		echo ${MKFS} ${dev}1
		${MKFS} ${dev}1
	done
}

perf_test() {
	${FIO}	--output=${OUTPUT} \
		--minimal \
		${CONFIG}

	# Field description.
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
}


usage() {
    echo "$0: [-r] [-p] [-f] [-t] [-m] [-a] [-P] [-h]"
    echo "    [--raid] [--part] [--fs] [--tune] [--mount] [--all] [--perf] [--help]"
    echo "  -r, --raid    Configure RAID"
    echo "  -p, --part    Create partitions"
    echo "  -f, --fs      Create file systems"
    echo "  -t, --tune    Tune file systems"
    echo "  -m, --mount   Mount file systems"
    echo "  -a, --all     Do everything"
    echo "  -P, --perf    Test disk performance"
    echo "  -h, --help    Display help message"
}

init_dev_map

main() {
	if [ $# -eq 0 ] ; then
	    usage
	    exit
	fi

	echo Welcome to the Mark6 disk management program
	echo
	echo This software has been developed by MIT Haystack Observatory and
	echo is released under the terms fo the GPL \(see LICENSE file\)
	    echo 
	    echo Please direct any questions to del@haystack.mit.edu
	    echo

	    while [ "$1" != "" ]; do
    		case $1 in
        	    -r | --raid )	mk_raid
			;;
		    -p | --part )	mk_part
			;;
		    -f | --fs )		mk_fs
			;;
		    -t | --tune )	tune_devs
			;;
		    -m | --mount )	mount_devs
			;;
		    -P | --perf )	perf_test
			;;
		    -a | --all )	mk_raid
					mk_part
					mk_fs
					tune_devs
					mount_devs
					;;
		    -h | --help )	usage
			exit
			;;
		    * )             usage
			exit 1
		esac
		shift
	    done
    }


# Kick off setup.
echo $*
main $*
