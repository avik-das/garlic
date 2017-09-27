#!/bin/sh

failed=0

cleanup() {
    rm -f test/tmp.result
    rm -f test/tmp.diff
}

clr_eol=`tput el`

log_info_clearable() {
    msg=$1
    # Don't output a newline since the line is meant to be cleared.
    printf "\033[1;32m$msg\033[0m"
}

clear_line() {
    printf "\r$clr_eol"
}

log_info() {
    msg=$1
    printf "\033[1;32m$msg\033[0m\n"
}

log_error() {
    msg=$1
    printf "\033[1;31m$msg\033[0m\n"
}

fail() {
    msg=$1
    log_error "$msg"

    failed=`expr $failed + 1`
}

log_info "Running unit tests"
echo
rspec test/rb
echo

rm -f test/tmp

num_success_tests=`ls -1 test/success/*.scm | wc -l`
num_failure_compile_tests=`ls -1 test/failure-compile/*.scm | wc -l`
num_total_tests=`expr $num_success_tests + $num_failure_compile_tests`

log_info "Running $num_total_tests functional tests"
echo

for testfile in test/success/*.scm; do
    log_info_clearable "Running \"$testfile\"..."

    ./garlic "$testfile" > test/tmp.result 2>&1

    if [ $? -ne 0 ]; then
        echo
        echo
        cat test/tmp.result

        fail "Compiling \"$testfile\" failed"
        echo
        continue
    fi

    ./main > test/tmp.result

    if [ $? -ne 0 ]; then
        echo
        echo
        cat test/tmp.result

        fail "Running \"$testfile\" failed"
        echo
        continue
    fi

    diff test/tmp.result "${testfile}.result" > test/tmp.diff

    if [ $? -ne 0 ]; then
        echo
        echo
        cat test/tmp.diff
        fail "Unexpected output running $testfile"
        echo
    fi

    clear_line
done

for testfile in test/failure-compile/*.scm; do
    log_info_clearable "Running \"$testfile\"..."

    ./garlic "$testfile" > test/tmp.result 2>&1

    if [ $? -eq 0 ]; then
        echo
        fail "Compiling \"$testfile\" unexpectedly succeeded"
        echo
    fi

    clear_line
done

for testfile in test/failure-runtime/*.scm; do
    log_info_clearable "Running \"$testfile\"..."

    ./garlic "$testfile" > test/tmp.result 2>&1

    if [ $? -ne 0 ]; then
        echo
        echo
        cat test/tmp.result

        fail "Compiling \"$testfile\" failed"
        echo
        continue
    fi

    ./main > test/tmp.result 2>&1

    if [ $? -eq 0 ]; then
        echo
        echo
        cat test/tmp.result

        fail "Running \"$testfile\" unexpectedly succeeded"
        echo
    fi

    clear_line
done

if [ $failed -eq 0 ]; then
    log_info "ALL TESTS SUCCEEDED"
else
    echo
    log_error "$failed TESTS FAILED"
fi

cleanup
