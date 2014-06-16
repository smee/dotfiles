#!/bin/bash
for f in $@; do unzip -v $f | awk '{print $1}'|tail -n 1; done | awk '{ s+=$1; } END { print s; }'
