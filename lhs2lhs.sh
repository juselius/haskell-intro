#!/bin/bash

for i in $*; do
    sed -i 's/^#/ #/' $i
done
