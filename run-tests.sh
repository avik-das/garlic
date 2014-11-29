#!/bin/sh

function cleanup {
    rm -f test/tmp.result
    rm -f test/tmp.diff
}

function fail {
    msg=$1
    echo "\033[1;31m$msg\033[0m"

    cleanup
    exit 1
}

rm -f test/tmp

for testfile in test/success/*.scm; do
    ./s-expr.rb "$testfile" && ./main > test/tmp.result

    if [ $? -ne 0 ]; then
        fail "Running $testfile failed"
    fi

    diff test/tmp.result "${testfile}.result" > test/tmp.diff

    if [ $? -ne 0 ]; then
        cat test/tmp.diff
        fail "Unexpected output running $testfile"
    fi
done

cleanup
