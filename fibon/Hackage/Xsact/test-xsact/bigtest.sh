#!/bin/bash

# bigtest.sh -- run a quick test for correctness
# assuming $DATA pointing to a directory containing data files

error=0
if test -e tmp-test; then
    echo "tmp-test exists, remove it"
    exit -1
fi

if test ! -e "$DATA"; then
    echo "Cannot find data directory ($DATA)!"
    echo "Please have \$DATA point at the location of the data files"
    exit -1
fi

mkdir tmp-test

echo "calculating bm-16.64"
./xsact -k 16 -n 64 -p 1 -L -x "$DATA"/b10000-masked.seq >tmp-test/bm-16.64.list 2> tmp-test/ERR1
echo "calculating bm-20.48"
./xsact -k 20 -n 48 -p 1 -L -x "$DATA"/b10000-masked.seq >tmp-test/bm-20.48.list 2> tmp-test/ERR1
echo "calculating ug-1.20.48"
./xsact -k 20 -n 48 -p 1 -x -L "$DATA"/ug-1.masked.seq >tmp-test/ug-20.48.list 2> tmp-test/ERR1
echo "calculating ug-1.20.48 (Unigene format)"
./xsact -k 20 -n 48 -p 1 -x -U "$DATA"/ug-1.masked.seq >tmp-test/ug-20.48.U 2> tmp-test/ERR1
echo "verifying directionality"
./xsact -k 20 -n 48 -p 1 -P tmp-test/ug-20.48.U 2> tmp-test/ERR1 | grep Rev > tmp-test/ug.rev

#todo: add testing of -U, -N and so on

b16="7a606b348fc2ed7493deeada4b1c7b03  tmp-test/bm-16.64.list"
b20="f88523dd4053d28e2ca870f1f1ba2725  tmp-test/bm-20.48.list"
u20="0b44274fd2714e6784534e35e267c22c  tmp-test/ug-20.48.list"

md5b16=`md5sum tmp-test/bm-16.64.list`
md5b20=`md5sum tmp-test/bm-20.48.list`
md5u20=`md5sum tmp-test/ug-20.48.list`

if test "$md5b16" = "$b16"; then
    echo "List test 16...OK"
else
    echo "List test 16...FAILED"
    error=1
fi

if test "$md5b20" = "$b20"; then
    echo "List test 20..OK"
else
    echo "List test 20..FAILED"
    error=1
fi

if test "$md5u20" = "$u20"; then
    echo "UG test.....OK"
else
    echo "UG test.....FAILED"
    error=1
fi

if egrep '(score:[1-9][0-9][0-9]|score:[6-9][0-9])' tmp-test/ug.rev; then
    echo ; echo "Bad matches found!"
    error=1
else
    echo "No significant reverse matches -- directions probably OK"
fi

if test $error -eq 1; then
    echo; echo "Some tests failed!"
    exit -1
else
    echo; echo "Tests passed, cleaning up"
    rm -rf tmp-test
fi
