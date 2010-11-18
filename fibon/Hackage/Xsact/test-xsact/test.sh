#!/bin/bash

# test.sh -- run a quick test for correctness

error=0
if test -e tmp-test; then
    echo "tmp-test exists, remove it"
    exit 99
fi

mkdir tmp-test

./xsact -k 10 -n 20 -L -s   test-xsact/test.seq -o tmp-test/test.list
./xsact -k 10 -n 20 -M      test-xsact/test.seq -o tmp-test/test.match
./xsact -k 10 -n 20 -M -p 1 test-xsact/test.seq -o tmp-test/test.match-p1
./xsact -k 10 -n 20 -M -p 3 test-xsact/test.seq -o tmp-test/test.match-p3
./xsact -k 10 -n 20 -P -x   test-xsact/test.seq -o tmp-test/test.pairs-ext

for a in A C G T; do
    ./xsact -k 10 -m $a test-xsact/test.seq
done;
./xsact -k 10 -n 20 -L -s -c 1 test-xsact/test.seq -o tmp-test/test.multi
./xsact -k 10 -n 20 -U -s      test-xsact/test.seq -o tmp-test/test.ug
./xsact -k 10 -n 20 -P         test-xsact/test.seq -o tmp-test/test.pairs
./xsact -k 10 -n 10 -N -s      test-xsact/test.seq -o tmp-test/test.newick

md5list=`md5sum tmp-test/test.list`
md5multi=`md5sum tmp-test/test.multi`
md5match=`md5sum tmp-test/test.match`
md5matchP1=`md5sum tmp-test/test.match-p1`
md5matchP3=`md5sum tmp-test/test.match-p3`
md5pairsExt=`md5sum tmp-test/test.pairs-ext`
md5ug=`md5sum tmp-test/test.ug`
md5newick=`md5sum tmp-test/test.newick`
md5pairs=`md5sum tmp-test/test.pairs`

list="90a411fb0fd4081d2d86ae0fbb5d35c7  tmp-test/test.list"
multi="90a411fb0fd4081d2d86ae0fbb5d35c7  tmp-test/test.multi"
match="19fa101bc4736ca9bfee09da046ddc2f  tmp-test/test.match"
newick="76238278aa7b7ae72c7fd30a42014073  tmp-test/test.newick"
pairs="47ccc03c19df224786791cebd1eb05bc  tmp-test/test.pairs"
ug="8eb0b0e179bc6dfafdc0c4a693e64642  tmp-test/test.ug"

if test "$md5list" = "$list"; then
    echo "List test...OK"
else
    echo "List test...FAILED"
    echo got $md5list
    echo wanted $list
    error=1
fi

if test "$md5multi" = "$multi"; then
    echo "Multi test...OK"
else
    echo "Multi test...FAILED"
    echo got $md5multi
    echo wanted $multi
    error=1
fi

if test "$md5match" = "$match"; then
    echo "Match test..OK"
else
    echo "Match test..FAILED"
    echo got $md5match
    echo wanted $match
    error=1
fi

if test "$md5matchP1" = "$match"-p1; then
    echo "Prefix 1 test..OK"
else
    echo "Prefix 1 test..FAILED"
    echo got $md5match
    echo wanted $match
    error=1
fi

if test "$md5matchP3" = "$match"-p3; then
    echo "Prefix 3 test..OK"
else
    echo "Prefix 3 test..FAILED"
    echo "got   " $md5match
    echo "wanted" $match
    error=1
fi

if test "$md5pairsExt" = "$pairs"-ext; then
    echo "ExtSort test..OK"
else
    echo "ExtSort test..FAILED"
    echo "got   " $md5pairsExt
    echo "wanted" $pairs
    error=1
fi

if test "$md5ug" = "$ug"; then
    echo "UG test.....OK"
else
    echo "UG test.....FAILED"
    echo "got   " $md5ug
    echo "wanted" $ug
    error=1
fi

if test "$md5pairs" = "$pairs"; then
    echo "Pairs test...OK"
else
    echo "Pairs test...FAILED"
    echo "got   " $md5pairs
    echo "wanted" $pairs
    error=1
fi

if test "$md5newick" = "$newick"; then
    echo "Newick test...OK"
else
    echo "Newick test...FAILED"
    echo "got   " $md5newick
    echo "wanted" $newick
    error=1
fi

if test $error -eq 1; then
    echo; echo "Some tests failed!"
    exit -1
else
    echo; echo "Tests passed, cleaning up"
    rm -rf tmp-test
fi

