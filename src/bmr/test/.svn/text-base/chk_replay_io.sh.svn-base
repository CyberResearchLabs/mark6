#!/bin/sh
#
# Script to check the i/o on bmr_replay
#
[ ${testverb-"0"} -eq 0 ] && verb=false || verb=true

bmr=../bmr_replay

rm -f ./EXPR_STN_scan_1.bmr
touch ./EXPR_STN_scan_1.bmr
rm -f replay.txt
( echo dump:replay.txt ; sleep 1 ; echo quit ) | $bmr 2>/dev/null

[ -f replay.txt ]

#
# eof
#
