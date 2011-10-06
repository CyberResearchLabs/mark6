#!/bin/sh
#
# Script to check the i/o on bmr_buffer
#
[ ${testverb-"0"} -eq 0 ] && verb=false || verb=true

bmr=../bmr_buffer

rm -f buffer.txt
( echo dump:buffer.txt ; sleep 1 ; echo quit ) | $bmr 2>/dev/null

[ -f buffer.txt ]

#
# eof
#
