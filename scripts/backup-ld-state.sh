#!/usr/bin/env bash

TARGET_REPO=~/dev/common-lisp/lambda-delta/build-archive

# Declare an array variable
declare -a FILES=("CMOS.RAM" "disks/disk.img" "ld.conf")

for file in "${FILES[@]}"
do
   cp "$file"  "$TARGET_REPO"
done



pushd .
cd "$TARGET_REPO"
git pull
git add -u
git commit -m "Updating revision"
git push
popd
