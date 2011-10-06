#!/bin/sh
#
# Script to check the i/o on bmr_record
#
[ ${testverb-"0"} -eq 0 ] && verb=false || verb=true

bmr=../bmr_record

rm -f record.txt
( echo dump:record.txt ; sleep 1 ; echo quit ) | $bmr 2>/dev/null

[ -f record.txt ]

#
# eof
#
