#!/bin/bash

for i in {3..25}
do
    export DAYNUM="$i"
    envsubst < Day.hs.template > "Day$i.hs"
done
