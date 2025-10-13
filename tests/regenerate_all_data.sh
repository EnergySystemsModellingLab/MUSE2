#!/bin/sh
set -euf

mydir=$(dirname "$0")
cd "$mydir"

echo Building MUSE2
examples=$(cargo run example list 2> /dev/null)

for example in $examples; do
    echo Generating data for example: $example

    # We only need debug files for the simple model
    extra_args=--overwrite
    if [ $example = simple ]; then
        extra_args+=" --debug-model"
    else
        extra_args+=" --debug-model=false"
    fi

    env MUSE2_LOG_LEVEL=off cargo run example run $extra_args -o "data/$example" "$example" 2> /dev/null
done
