#!/bin/bash

# time.sh -- measure time to perform various clusterings
# assuming $DATA pointing to a directory containing data files

TIME="/usr/bin/time -a -p -o test-times"

if test ! -e "$DATA"; then
    echo "Cannot find data directory ($DATA)!"
    echo "Please have \$DATA point at the location of the data files"
    exit -1
fi

echo -n "===== Timing run started at " >> test-times
date >> test-times

echo  "calculating bm-16.64:   "  >> test-times
$TIME ./xsact -k 16 -n 64 -p 1 -L "$DATA"/b10000-masked.seq > /dev/null
echo  "calculating bm-20.48:   "  >> test-times
$TIME ./xsact -k 20 -n 48 -p 1 -L "$DATA"/b10000-masked.seq > /dev/null

rm -f "$DATA"/b10000-masked.seq-20_M*
echo  "calculating bm-20.48 ext sorting:   "  >> test-times
$TIME ./xsact -k 20 -n 48 -p 1 -L -x "$DATA"/b10000-masked.seq > /dev/null
echo  "calculating bm-20.48 from pregen matchlist:   "         >> test-times
$TIME ./xsact -k 20 -n 48 -p 1 -L -x "$DATA"/b10000-masked.seq > /dev/null
echo  "calculating bm-20.48 from pregen matchlist (-U):   "    >> test-times
$TIME ./xsact -k 20 -n 48 -p 1 -U -x "$DATA"/b10000-masked.seq > /dev/null
echo  "calculating bm-20.48 from pregen matchlist (-H):   "    >> test-times
$TIME ./xsact -k 20 -n 48 -p 1 -H -x "$DATA"/b10000-masked.seq > /dev/null

echo  "calculating ug-1.16.64: "  >> test-times
$TIME ./xsact -k 16 -n 64 -p 1 -L "$DATA"/ug-1.masked.seq > /dev/null
echo  "calculating ug-1.20.48: "  >> test-times
$TIME ./xsact -k 20 -n 48 -p 1 -L "$DATA"/ug-1.masked.seq > /dev/null

cat test-times
