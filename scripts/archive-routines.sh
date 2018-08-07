#!/usr/bin/env bash


TARGET_REPO=~/dev/common-lisp/lambda-delta/build-archive

# Declare an array with our file names.
declare -a FILES=("CMOS.RAM" "disks/disk.img" "ld.conf")

function backup-ld-state {
    pushd .
    # To project's git articfacts archive
    cd "$TARGET_REPO"
    git pull
    popd
    # Push again
    pushd .
    # Now to project root directory
    cd ..
    for file in "${FILES[@]}"
    do
	echo "copying: $file"
	cp "$file"  "$TARGET_REPO"
    done
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

# Restores whatever is current in the rope
function restore-ld-state {
    pushd .
    cd ..
    for file in "${FILES[@]}"
    do
	FROM_FILE=$(basename $file)
	echo "cp $TARGET_REPO/$FROM_FILE  $file"
	cp "$TARGET_REPO/$FROM_FILE"  "$file"
    done
    popd
}
