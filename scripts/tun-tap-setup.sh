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

    # Note, they can be in different namespaces too, allowing for
    # network namespace 'tunnel'.
    # ip -4 link set veth2 netns lambda-delta

    # lambda-a
    ip -4 tuntap add lambda-a-tap mode tap
    ip -4 link set dev lambda-a-tap up

    # lambda-b
    ip -4 tuntap add lambda-b-tap mode tap
    ip -4 link set dev lambda-b-tap up


    ip -4 link set dev veth1 up
    ip -4 link set dev veth2 up

    ip -4 link add ldbridge type bridge
    ip -4 link set lambda-a-tap master ldbridge
    ip -4 link set lambda-b-tap master ldbridge
    ip -4 addr add 100.0.0.11/24 dev ldbridge
    ip -4 addr add 100.0.0.2/24 dev veth2
    ip -4 link set ldbridge up
    ip -4 link set veth1 master ldbridge

}

function tear-down {
    sudo ip netns exec lambda-delta bash
    ip link set dev veth1 down
    ip link set veth2 down
    ip link set ldbridge down
    ip link set dev lambda-a-tap down
    ip link set dev lambda-b-tap down
    ip link delete ldbridge type bridge
    ip tuntap delete lambda-a-tap mode tap
    ip tuntap delete lambda-b-tap mode tap
    ip link delete dev veth1
}

# create local bridge
ip -4 link add local-bridge type bridge




function local-bridge {
    NNS=lambda-delta
    ip netns add $NNS
    ip link add va-local type veth peer name vb-local
    ip link set va-local up
    ip tuntap add tap-local mode tap user root
    ip link set tap-local up

    ip link add br-local type bridge
    ip link set tap-local master br-local

    ip link set va-local master br-local
    ip addr add 10.0.0.1/24 dev br-local
    ip link set br-local up

    ip link set vb-local netns lambda-delta

    # ip netns exec $NNS ip addr add 10.0.0.2/24 dev veth-b$NNS

    # ip netns exec $NNS ip link set veth-b$NNS up
    # ip netns exec $NNS ip link set dev lo up
}

function exec-in-ns {
    ip addr add 10.0.0.3/24 dev vb-local
    ip link set vb-local up
    ip link set dev lo up
}
