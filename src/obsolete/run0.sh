#!/bin/bash

export LD_LIBRARY_PATH=/home/dlapsley/mark6/src/extern/PF_RING-4.7.0/userland/lib

# Globals
EXEC=./net2disk

# IRQ "MAP"
ETH2_IRQ=63
ETH3_IRQ=65
ETH4_IRQ=66
ETH5_IRQ=64

# Set local IRQ and AFFINITY
ETH_IRQ=${ETH2_IRQ}
ETH_MASK=2
ETH_AFFINITY=1
INTERFACE=eth2
DISK=disk0
RING_BUFFERS=128

# Setup IRQ AFFINITY
echo cat /proc/irq/${ETH_IRQ}/smp_affinity
cat /proc/irq/${ETH_IRQ}/smp_affinity
echo ${ETH_MASK} > /proc/irq/${ETH_IRQ}/smp_affinity
echo cat /proc/irq/${ETH_IRQ}/smp_affinity
cat /proc/irq/${ETH_IRQ}/smp_affinity


# Run!
${EXEC} \
	--interfaces ${INTERFACE} \
	--capture_files /mnt/${DISK}/cap.m6 \
	--smp_affinities ${ETH_AFFINITY} \
	--ring_buffers ${RING_BUFFERS} \
	--write_blocks ${RING_BUFFERS}
