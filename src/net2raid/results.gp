#!/usr/bin/gnuplot

set terminal wxt

FW_ETH2='/opt/mit/mark6/log/fw_eth2.csv'
FW_ETH3='/opt/mit/mark6/log/fw_eth3.csv'
FW_ETH4='/opt/mit/mark6/log/fw_eth4.csv'
FW_ETH5='/opt/mit/mark6/log/fw_eth5.csv'

NR_ETH2='/opt/mit/mark6/log/nr_eth2.csv'
NR_ETH3='/opt/mit/mark6/log/nr_eth3.csv'
NR_ETH4='/opt/mit/mark6/log/nr_eth4.csv'
NR_ETH5='/opt/mit/mark6/log/nr_eth5.csv'

set title 'Ethernet2 FileWriter'
set xlabel 'Time(s)'
plot FW_ETH2 using 1:5 t 'inst bit rate', FW_ETH2 using 1:6 t 'life bit rate', FW_ETH2 using 1:7 t 'avg bit rate'
pause -1

set terminal pdf
set output "/opt/mit/mark6/log/eth2_fw.pdf"
replot
set terminal wxt

set title 'Ethernet2 NetReader'
set xlabel 'Time(s)'
plot NR_ETH2 using 1:5 t 'inst bit rate', NR_ETH2 using 1:6 t 'life bit rate', NR_ETH2 using 1:7 t 'avg bit rate'
pause -1

set terminal pdf
set output "/opt/mit/mark6/log/eth2_nr.pdf"
replot
set terminal wxt

set title 'Instantaneous NR Bit Rate'
set xlabel 'Time(s)'
plot NR_ETH2 using 1:7 t 'eth2' with lines, NR_ETH3 using 1:7 t 'eth3' with lines, NR_ETH4 using 1:7 t 'eth4' with lines, NR_ETH5 using 1:7 t 'eth5' with lines
pause -1

set terminal pdf
set output "/opt/mit/mark6/log/nr.pdf"
replot
set terminal wxt

set title 'Instantaneous FW Bit Rate'
set xlabel 'Time(s)'
plot FW_ETH2 using 1:7 t 'eth2' with lines, FW_ETH3 using 1:7 t 'eth3' with lines, FW_ETH4 using 1:7 t 'eth4' with lines, FW_ETH5 using 1:7 t 'eth5' with lines
pause -1

set terminal pdf
set output "/opt/mit/mark6/log/fw.pdf"
replot
set terminal wxt

VMSTAT_DATA='/opt/mit/mark6/log/vmstat.log'

set title 'Procs'
set xlabel 'Time(s)'
set ylabel 'Bytes'
plot VMSTAT_DATA every ::3 using 3 t 'r' with lines,\
	VMSTAT_DATA every ::3 using 4 t 'b' with lines
pause -1

set terminal pdf
set output "/opt/mit/mark6/log/procs.pdf"
replot
set terminal wxt


set title 'Memory'
set xlabel 'Time(s)'
set ylabel 'Bytes'
plot VMSTAT_DATA every ::3 using 3 t 'swpd' with lines,\
	VMSTAT_DATA every ::3 using 4 t 'free' with lines,\
	VMSTAT_DATA every ::3 using 5 t 'buff' with lines,\
	VMSTAT_DATA every ::3 using 6 t 'cache' with lines
pause -1

set terminal pdf
set output "/opt/mit/mark6/log/mem.pdf"
replot
set terminal wxt

set title 'CPU'
set xlabel 'Time(s)'
set ylabel '%'
plot VMSTAT_DATA every ::3 using 13 t 'us' with lines,\
	VMSTAT_DATA every ::3 using 14 t 'sy' with lines,\
	VMSTAT_DATA every ::3 using 15 t 'id' with lines,\
	VMSTAT_DATA every ::3 using 16 t 'wa' with lines
pause -1
set terminal pdf
set output "/opt/mit/mark6/log/cpu.pdf"
replot
set terminal wxt

set title 'Swap'
set xlabel 'Time(s)'
set ylabel 'Pages/s'
plot VMSTAT_DATA every ::3 using 7 t 'si' with lines,\
	VMSTAT_DATA every ::3 using 8 t 'so' with lines
pause -1
set terminal pdf
set output "/opt/mit/mark6/log/swap.pdf"
replot
set terminal wxt

set title 'IO'
set xlabel 'Time(s)'
set ylabel 'Blocks/s'
plot VMSTAT_DATA every ::3 using 9 t 'bi' with lines,\
	VMSTAT_DATA every ::3 using 10 t 'bo' with lines
pause -1
set terminal pdf
set output "/opt/mit/mark6/log/io.pdf"
replot
set terminal wxt

set title 'Interrupts and Context Switches'
set xlabel 'Time(s)'
set ylabel 'events/s'
plot VMSTAT_DATA every ::3 using 11 t 'in' with lines,\
	VMSTAT_DATA every ::3 using 12 t 'cs' with lines
pause -1
set terminal pdf
set output "/opt/mit/mark6/log/int.pdf"
replot
set terminal wxt


