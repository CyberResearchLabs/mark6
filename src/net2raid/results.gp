#!/usr/bin/gnuplot

FW_ETH2='/opt/mit/mark6/log/fw_eth2.csv'
FW_ETH3='/opt/mit/mark6/log/fw_eth3.csv'
FW_ETH4='/opt/mit/mark6/log/fw_eth4.csv'
FW_ETH5='/opt/mit/mark6/log/fw_eth5.csv'

NR_ETH2='/opt/mit/mark6/log/nr_eth2.csv'
NR_ETH3='/opt/mit/mark6/log/nr_eth3.csv'
NR_ETH4='/opt/mit/mark6/log/nr_eth4.csv'
NR_ETH5='/opt/mit/mark6/log/nr_eth5.csv'

plot FW_ETH2 using 1:5 t 'inst bit rate', FW_ETH2 using 1:6 t 'life bit rate', FW_ETH2 using 1:7 t 'avg bit rate'
pause -1

plot NR_ETH2 using 1:5 t 'inst bit rate', NR_ETH2 using 1:6 t 'life bit rate', NR_ETH2 using 1:7 t 'avg bit rate'
pause -1

set title 'Instantaneous NR Bit Rate'
plot NR_ETH2 using 1:7 t 'eth2' with lines, NR_ETH3 using 1:7 t 'eth3' with lines, NR_ETH4 using 1:7 t 'eth4' with lines, NR_ETH5 using 1:7 t 'eth5' with lines
pause -1


set title 'Instantaneous FW Bit Rate'
plot FW_ETH2 using 1:7 t 'eth2' with lines, FW_ETH3 using 1:7 t 'eth3' with lines, FW_ETH4 using 1:7 t 'eth4' with lines, FW_ETH5 using 1:7 t 'eth5' with lines
pause -1
