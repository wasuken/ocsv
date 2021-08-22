#!/bin/bash

echo "a,b,c"
for i in {0..209243}; do
   echo "${RANDOM},${RANDOM},${RANDOM}"
done
