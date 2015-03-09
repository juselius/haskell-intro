#!/bin/bash

for i in $*; do
    sed -i 's/^#/ #/; s/^>$/> /' $i
done
