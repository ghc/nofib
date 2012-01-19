#!/bin/bash

# test.sh -- run a quick test for correctness

tests="loop ignN jump single repword"

error=0
if test -e tmp-test; then
    echo "tmp-test exists, remove it"
    exit -1
fi

mkdir tmp-test

# commands
for a in $tests; do
  echo "++++++ Performing" $a "++++++" >> tmp-test/xtract.res
  ./xtract -k 5 -C test-xtract/$a.seq >> tmp-test/xtract.res
done

# gen results

if diff -u test-xtract/xtract.std tmp-test/xtract.res; then
    echo "Tests passed, cleaning up"
    rm -rf tmp-test
else
    echo "Error occurred"
    exit -1
fi
