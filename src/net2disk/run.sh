#!/bin/bash

export LD_LIBRARY_PATH=/home/dlapsley/mark6/src/extern/PF_RING-4.7.0/userland/lib

# Globals
EXEC=./net2disk
ETHTOOL=/sbin/ethtool

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

# /etc/modules
pf_ring transparent_mode=0 min_num_slots=4096 enable_ip_defrag=1

# IRQ "MAP"
ETH2_IRQ=63
ETH3_IRQ=65
ETH4_IRQ=66
ETH5_IRQ=64
MEGASAS1_IRQ=24
MEGASAS1_IRQ=34

RING_BUFFERS=256

# Setup IRQ AFFINITY (echoing a CPU mask to set affinity)
echo 8 > /proc/irq/${ETH2_IRQ}/smp_affinity
echo cat /proc/irq/${ETH2_IRQ}/smp_affinity

echo 8 > /proc/irq/${ETH3_IRQ}/smp_affinity
echo cat /proc/irq/${ETH3_IRQ}/smp_affinity

echo 10 > /proc/irq/${ETH4_IRQ}/smp_affinity
echo cat /proc/irq/${ETH4_IRQ}/smp_affinity

echo 10 > /proc/irq/${ETH5_IRQ}/smp_affinity
echo cat /proc/irq/${ETH5_IRQ}/smp_affinity

echo 2 > /proc/irq/${MEGASAS1_IRQ}/smp_affinity
echo cat /proc/irq/${MEGASAS1_IRQ}/smp_affinity

echo 4 > /proc/irq/${MEGASAS2_IRQ}/smp_affinity
echo cat /proc/irq/${MEGASAS2_IRQ}/smp_affinity

# Run! 
# Note that SMP affinities are ordinals (not mask). CPU numbering from 0.
${EXEC} \
    --interfaces eth2 eth3 eth4 eth5 \
    --capture_files /mnt/disk2/cap.m6 /mnt/disk3/cap.m6 /mnt/disk0/cap.m6 /mnt/disk1/cap.m6 \
    --smp_affinities 5 5 6 6 \
    --ring_buffers ${RING_BUFFERS} \
    --write_blocks ${RING_BUFFERS}

    # --capture_files /mnt/disk2/cap.m6 /mnt/disk3/cap.m6 /mnt/disk0/cap.m6 /mnt/disk1/cap.m6 \
    # --capture_files /mnt/disk0/cap.m6 /mnt/disk1/cap.m6 /mnt/disk2/cap.m6 /mnt/disk3/cap.m6 \
