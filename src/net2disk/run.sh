#!/bin/bash

export LD_LIBRARY_PATH=/home/dlapsley/mark6/src/extern/PF_RING-4.7.0/userland/lib

# Globals
EXEC=./net2disk

# IRQ "MAP"
ETH2_IRQ=63
ETH3_IRQ=65
ETH4_IRQ=66
ETH5_IRQ=64

RING_BUFFERS=128

# Setup IRQ AFFINITY (echoing a CPU mask to set affinity)
echo 2 > /proc/irq/${ETH2_IRQ}/smp_affinity
echo cat /proc/irq/${ETH2_IRQ}/smp_affinity

echo 2 > /proc/irq/${ETH3_IRQ}/smp_affinity
echo cat /proc/irq/${ETH3_IRQ}/smp_affinity

echo 4 > /proc/irq/${ETH4_IRQ}/smp_affinity
echo cat /proc/irq/${ETH4_IRQ}/smp_affinity

echo 4 > /proc/irq/${ETH5_IRQ}/smp_affinity
echo cat /proc/irq/${ETH5_IRQ}/smp_affinity

# Run! 
# Note that SMP affinities are ordinals (not mask). CPU numbering from 0.
${EXEC} \
    --interfaces eth2 eth3 eth4 \
    --capture_files \
      /mnt/disk0/cap.m6 /mnt/disk1/cap.m6 \
      /mnt/disk2/cap.m6 \
    --smp_affinities 1 1 2 \
    --ring_buffers ${RING_BUFFERS} \
    --write_blocks ${RING_BUFFERS}
