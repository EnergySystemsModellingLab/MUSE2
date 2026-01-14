#!/bin/sh
set -euf

mydir=$(dirname "$0")
cd "$mydir"

echo Building MUSE2
cargo build 2> /dev/null

if [[ $# -gt 0 ]]; then
    examples=$@
else
    examples=$(cargo run example list 2> /dev/null)
fi

for example in $examples; do
    # Skip the circularity example
    if [ "$example" = circularity ]; then
        continue
    fi

    echo Generating data for example: $example

    # We only need debug files for the simple model
    extra_args=--overwrite
    if [ $example = simple ]; then
        extra_args="$extra_args --debug-model"
    else
        extra_args="$extra_args --debug-model=false"
    fi

    env MUSE2_LOG_LEVEL=off cargo run example run $extra_args -o "data/$example" "$example" 2> /dev/null
done
