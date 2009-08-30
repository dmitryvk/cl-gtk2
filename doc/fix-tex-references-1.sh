#!/bin/sh
file="$1"
(egrep '<meta http-equiv="refresh"' -q < "$file") && \
sed -e 's/%/_00/g' -i "$file"
true
