#!/bin/bash

path=/tmp/blocks_unprll
IFS=$'\n'
read l < $path
timeprev=${l/ *}
for l in $(<$path); do
    time=${l/ *}
    echo $time $((time-timeprev))
    timeprev=$time
done

