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
ETH_IRQ=${ETH4_IRQ}
ETH_MASK=8
ETH_MASK=4
ETH_AFFINITY=2
INTERFACE=eth4
DISK=disk2
RING_BUFFERS=128

# Setup IRQ AFFINITY
echo cat /proc/irq/${ETH_IRQ}/smp_affinity
cat /proc/irq/${ETH_IRQ}/smp_affinity
echo ${ETH_MASK} > /proc/irq/${ETH_IRQ}/smp_affinity
echo cat /proc/irq/${ETH_IRQ}/smp_affinity
cat /proc/irq/${ETH_IRQ}/smp_affinity


# Run!
${EXEC} \
	--interface ${INTERFACE} \
	--capture_file /mnt/${DISK}/cap.m6 \
	--smp_affinity ${ETH_AFFINITY} \
	--ring_buffers ${RING_BUFFERS} \
	--write_blocks ${RING_BUFFERS}
