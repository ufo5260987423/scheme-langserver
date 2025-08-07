#!/usr/bin/env bash

TARGET_DIR="analysis/identifier/rules"
TARGET_COMMENT="; reference-identifier-type include"

find "$TARGET_DIR" -type f -name "*.sls" |\
xargs grep --no-filename -A 1 "$TARGET_COMMENT" |\
grep -v "$TARGET_COMMENT" |\
grep -v -- "^--$" |\
grep '^;' |\
sed 's/^;//' |\
tr ' ' '\n' |\
grep -v '^$' |\
sort -u 

# |\ awk '{print "(equal? \x27" $1 " (identifier-reference-type identifier))"}' 