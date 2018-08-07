


# Paste the following into /etc/network/interfaces
#
# our lamdadelta device is ldtap
# for original see: https://help.ubuntu.com/community/Network%20Bridge%20with%20a%20Tap%21
# where is the log file for this?
pre-up tunctl -t ldtap -u rett -g rett
pre-up ip link set dev eno1 down
pre-up brctl addbr br0
pre-up brctl addif br0 eno1
pre-up brctl addif br0 ldtap
pre-up ip link set dev ldtap up
up chmod 0666 /dev/net/tun
post-down ip link set dev eno1 down
post-down ip link set dev ldtap dpwn
post-down ip link set dev br0 down
post-down brctl delif br0 eno1
post-down brctl delif br0 ldtap
post-down brctl delbr br0



# additional things to call:
