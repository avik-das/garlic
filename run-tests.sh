#!/bin/sh

function cleanup {
    rm -f test/tmp.result
    rm -f test/tmp.diff
}

clr_eol=`tput el`

function log_info_clearable {
    msg=$1
    # Don't output a newline since the line is meant to be cleared.
   printf "\033[1;32m$msg\033[0m"
}

function clear_line {
    printf "\r$clr_eol"
}

function log_info {
    msg=$1
    printf "\033[1;32m$msg\033[0m\n"
}

function fail {
    msg=$1
    printf "\033[1;31m$msg\033[0m\n"

    cleanup
    exit 1
}

rm -f test/tmp

num_success_tests=`ls -1 test/success/*.scm | wc -l`
num_failure_tests=`ls -1 test/failure/*.scm | wc -l`
num_total_tests=`expr $num_success_tests + $num_failure_tests`

log_info "Running $num_total_tests tests"
echo

for testfile in test/success/*.scm; do
    log_info_clearable "Running \"$testfile\"..."

    ./s-expr.rb "$testfile"

    if [ $? -ne 0 ]; then
        echo
        fail "Compiling \"$testfile\" failed"
    fi

    ./main > test/tmp.result

    if [ $? -ne 0 ]; then
        echo
        fail "Running \"$testfile\" failed"
    fi

    diff test/tmp.result "${testfile}.result" > test/tmp.diff

    if [ $? -ne 0 ]; then
        echo
        cat test/tmp.diff
        fail "Unexpected output running $testfile"
    fi

    clear_line
done

for testfile in test/failure/*.scm; do
    log_info_clearable "Running \"$testfile\"..."

    (./s-expr.rb "$testfile" && ./main) > test/tmp.result 2>&1

    if [ $? -eq 0 ]; then
        echo
        fail "Compiling and running \"$testfile\" unexpectedly succeeded"
    fi

    clear_line
done

log_info "ALL TESTS SUCCEEDED"

cleanup
