#!/bin/bash

# export LD_LIBRARY_PATH=/home/dlapsley/mark6/src/extern/PF_RING-4.7.0/userland/lib
export LD_LIBRARY_PATH=/opt/haystack/lib

# Globals
EXEC=/opt/haystack/bin/net2disk
ETHTOOL=/sbin/ethtool
BLOCKDEV=/sbin/blockdev

# Configure ethernet drivers
${ETHTOOL} -K eth2 gro on
${ETHTOOL} -K eth3 gro on
${ETHTOOL} -K eth4 gro on
${ETHTOOL} -K eth5 gro on

# Offload parameters for eth2:
# rx-checksumming: on
# tx-checksumming: on
# scatter-gather: on
# tcp-segmentation-offload: on
# udp-fragmentation-offload: off
# generic-segmentation-offload: on
# generic-receive-offload: off
# large-receive-offload: on
# ntuple-filters: off
# receive-hashing: off

# RAID settings
${BLOCKDEV} --setra 16384 /dev/sda
${BLOCKDEV} --setra 16384 /dev/sdb
${BLOCKDEV} --setra 16384 /dev/sdc
${BLOCKDEV} --setra 16384 /dev/sdd

echo deadline > /sys/block/sda/queue/scheduler
echo deadline > /sys/block/sdb/queue/scheduler
echo deadline > /sys/block/sdc/queue/scheduler
echo deadline > /sys/block/sdd/queue/scheduler

echo 512 > /sys/block/sda/queue/nr_requests
echo 512 > /sys/block/sdb/queue/nr_requests
echo 512 > /sys/block/sdc/queue/nr_requests
echo 512 > /sys/block/sdd/queue/nr_requests

# /etc/modules
# pf_ring transparent_mode=0 min_num_slots=4096 enable_ip_defrag=1

# IRQ "MAP"
ETH2_IRQ=63
ETH3_IRQ=65
ETH4_IRQ=64
ETH5_IRQ=66
MEGASAS1_IRQ=24
MEGASAS2_IRQ=35

RING_BUFFERS=512
# RING_BUFFERS=1024
# RING_BUFFERS=2048

# Setup IRQ AFFINITY (echoing a CPU mask to set affinity)
echo 1 > /proc/irq/${ETH2_IRQ}/smp_affinity
echo cat /proc/irq/${ETH2_IRQ}/smp_affinity

echo 1 > /proc/irq/${ETH3_IRQ}/smp_affinity
echo cat /proc/irq/${ETH3_IRQ}/smp_affinity

echo 4 > /proc/irq/${ETH4_IRQ}/smp_affinity
echo cat /proc/irq/${ETH4_IRQ}/smp_affinity

echo 4 > /proc/irq/${ETH5_IRQ}/smp_affinity
echo cat /proc/irq/${ETH5_IRQ}/smp_affinity

# CPU0
echo 1 > /proc/irq/${MEGASAS1_IRQ}/smp_affinity
echo cat /proc/irq/${MEGASAS1_IRQ}/smp_affinity

# CPU2
echo 4 > /proc/irq/${MEGASAS2_IRQ}/smp_affinity
echo cat /proc/irq/${MEGASAS2_IRQ}/smp_affinity

# Run! 
# Note that SMP affinities are ordinals (not mask). CPU numbering from 0.
for m in disk0 disk1 disk2 disk3
do
	fallocate -l 2400G /mnt/${m}/cap.m6
done

${EXEC} \
    --interfaces eth2 eth3 eth4 eth5 \
    --capture_files /mnt/disk0/cap.m6 /mnt/disk1/cap.m6 /mnt/disk2/cap.m6 /mnt/disk3/cap.m6 \
    --smp_affinities 1 1 3 3 \
    --ring_buffers ${RING_BUFFERS} \
    --write_blocks ${RING_BUFFERS}

    # --interfaces eth2 eth3 eth4 eth5 \
    # --capture_files /mnt/disk0/cap.m6 /mnt/disk1/cap.m6 /mnt/disk2/cap.m6 /mnt/disk3/cap.m6 \
    # --capture_files /mnt/disk0/cap.m6 /mnt/disk1/cap.m6 /mnt/disk2/cap.m6 /mnt/disk3/cap.m6 \
    # --capture_files /mnt/disk0/cap.m6 /mnt/disk1/cap.m6 /mnt/disk2/cap.m6 /mnt/disk3/cap.m6 \
