#!/bin/bash

s=18400000
u=20
l=80

t=20

c=1
while [ $c -lt $t ]; do
    part=$(bc <<< "scale=2; 20/$c")
    # echo -n $((20/c)) " "
    echo -n "$part "
    printf "%'.3f\n" $((s/100*80))
    s=$((s/100*80))
    c=$((c+1))
done
