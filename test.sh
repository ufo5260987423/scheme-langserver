#!/usr/bin/env bash
source .akku/bin/activate

skip=(
    "./tests/output-identifier-types.sps" 
    "./tests/parallel-log-debug.sps" 
    "./tests/log-debug.sps" 
    "./tests/performance.sps" 
)

success=0

for test in $(find ./tests | grep ".sps$")
do
    if [[ "${skip[@]}" =~ $test ]]; then continue; fi
    scheme --quiet $test
    success=$(($? || $success))
done;

exit $success