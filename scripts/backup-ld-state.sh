#!/usr/bin/env bash

TARGET_REPO=~/dev/common-lisp/lambda-delta/build-archive

declare -a FILES=("CMOS.RAM" "disks/disk.img" "ld.conf")

function backup-ld-state {
    # Declare an array variable
    pushd .
    cd "$TARGET_REPO"
    git pull
    popd
    for file in "${FILES[@]}"
    do
	echo "copying: $file"
	cp "$file"  "$TARGET_REPO"
    done
    pushd .
    cd "$TARGET_REPO"
    git add -u
    git commit -m "Updating revision"
    git push
    popd
}

function delete-ld-state {
    pushd .
    cd ..
    for file in "${FILES[@]}"
    do
	rm -f "$file"
    done
    popd
}
