# https://superuser.com/questions/764986/howto-setup-a-veth-virtual-network/765078

# Note these are all temporary, i.e. will disapear after a reboot, for
# better or worse.
function setup-interfaces {
    sudo ip netns add lambda-delta
    # Test
    # sudo ip netns exec lambda-delta ip link ls
    sudo ip -4 netns exec lambda-delta bash
    # To verify
    # sudo ip netns ls
    ip -4 link add dev veth1 type veth peer name veth2
    # sudo ip link set veth1 netns lambda-delta

    # Note, they can be in different namespaces too, allowing for
    # network namespace 'tunnel'.
    # ip -4 link set veth2 netns lambda-delta

    ip -4 tuntap add ldtap mode tap
    ip -4 link set dev ldtap up

    ip -4 link set dev veth1 up
    ip -4 link set dev veth2 up

    ip -4 link add ldbridge type bridge
    ip -4 link set ldtap master ldbridge
    ip -4 addr add 100.0.0.11/24 dev ldbridge
    ip -4 addr add 10.0.0.2/24 dev veth2
    ip -4 link set ldbridge up
    ip -4 link set veth1 master ldbridge

}

function tear-down {
    sudo ip netns exec lambda-delta bash
    ip link set dev veth1 down
    ip link set veth2 down
    ip link set ldbridge down
    ip link set dev ldtap down
    ip link delete ldbridge type bridge
    ip tuntap delete ldtap mode tap
    ip link delete dev veth1
}



# sniff(iface="ldtap", prn=lambda x: x.summary())
# sniff(iface="ldtap", prn=lambda x: x)
#  sniff(iface="ldtap", prn=lambda x: x.show())
