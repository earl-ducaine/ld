#!/usr/bin/env bash

# TAG_VERSION="instr-0.00.00"
# ARCHIVE_PROJECT_ROOT="/home/rett/dev/common-lisp/lambda-delta/build-archive/$TAG_VERSION"

# # Create directory that corresponds to tagged version
# mkdir -p "$ARCHIVE_PROJECT_ROOT"


# PROJECTS_PATHNAMES=("disks" "ld.conf" "tapes" "roms" "RTC.RAM" "CMOS.RAM" "src/lam" "logs")

# for pathname in "${PROJECTS_PATHNAMES[@]}"
# do
#     cp -r "$pathname" "$ARCHIVE_PROJECT_ROOT"
# done

# echo "(:tag-version \"$TAG_VERSION\"" > "$ARCHIVE_PROJECT_ROOT/build-manifest.lisp"

autoreconf -i -f
./configure --without-SDL2 --enable-config-2x2=yes
make
mkdir -p disks
touch disks/disk.img
mkdir roms

cp ~/.cache/lambda-delta/roms/memory/2243902_OF_27S291.BIN roms/MEM.ROM
cp /home/rett/.cache/lambda-delta/roms/SDU.ROM roms
cp /home/rett/.cache/lambda-delta/roms/VCMEM.ROM roms

# Silence is a good thing!
md5sum -c --quiet roms-checksums.md5sum

cp ~/.cache/lambda-delta/dj_full/l/ubin/bootstrap.lam-uload.7 bootstrap.lam-uload
cp ~/.cache/lambda-delta/dj_full/l/ubin/ulambda.lmc-sym.1762 ulambda.lmc-sym



# Overwrite without warning!
cat > ld.conf << EOT
# LambdaDelta configuration file
ether_iface ldtap
ether_addr 00:02:9C:55:89:C6
disk 0 disks/disk.img
sdu_switch 0
EOT


# if [ "$CSUM1" = "$CSUM2" ]; then
# echo 'verification successful!'
# else
# echo 'verification failed!'
# fi


# Run, but not in network name space.  the lam emulator opens up a
# telnet port 3627 on local host and hooks that up to the virtual lisp
# machine's serial port.


sudo src/lam > lam.log
