#!/bin/sh
dir="$1"
for file in $dir/*.html; do
    ./fix-tex-references-1.sh "$file"
done
