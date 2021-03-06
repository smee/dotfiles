#!/bin/sh
git fsck --unreachable
git reflog expire --expire=0 --all
git repack -a -d -l
git prune
git gc --aggressive
