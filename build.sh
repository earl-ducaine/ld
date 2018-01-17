#!/usr/bin/env bash

TAG_VERSION="instr-0.00.00"
ARCHIVE_PROJECT_ROOT="/home/rett/dev/common-lisp/lambda-delta/build-archive/$TAG_VERSION"

# Create directory that corresponds to tagged version
mkdir -p "$ARCHIVE_PROJECT_ROOT"


PROJECTS_PATHNAMES=("disks" "ld.conf" "tapes" "roms" "RTC.RAM" "CMOS.RAM" "src/lam" "logs")

for pathname in "${PROJECTS_PATHNAMES[@]}"
do
    cp -r "$pathname" "$ARCHIVE_PROJECT_ROOT"
done

echo "(:tag-version \"$TAG_VERSION\"" > "$ARCHIVE_PROJECT_ROOT/build-manifest.lisp"
