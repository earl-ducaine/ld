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
    ip -4 tuntap add lambda-a-tap mode tap user rett
    ip -4 link set dev lambda-a-tap up

    # lambda-b
    ip -4 tuntap add lambda-b-tap mode tap user rett
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
    ip netns add ns-test
    ip link add vea-test type veth peer name veb-test
    ip link set vea-test up
    ip tuntap add tap-test mode tap user rett
    ip link set tap-test up
    ip link add br-test type bridge
    ip link set tap-test master br-test
    ip link set vea-test master br-test


    ip addr add 10.0.0.1/24 dev br-test
    ip link set br-test up

    ip link set veb-test netns ns-test

    # ip netns exec $NNS ip addr add 10.0.0.2/24 dev veth-b$NNS

    # ip netns exec $NNS ip link set veth-b$NNS up
    # ip netns exec $NNS ip link set dev lo  up
}

functtion create-test-environment {
    ip netns add ns-test
    ip link add vea-test type veth peer name veb-test
    ip link set vea-test up
    ip tuntap add tap-test mode tap user rett
    ip link set tap-test up
    ip link add br-test type bridge
    ip link set tap-test master br-test
    ip link set vea-test master br-test
    ip addr add 10.0.0.1/24 dev br-test
    ip link set br-test up
    ip link set veb-test netns ns-test
    ip netns exec ns-test ./tun-tap-test-config-in-ns.sh
}

function delete-test-environment {
    ip link set dev vea-test down
    ip link set br-test down
    ip link set dev tap-test down
    sudo ip netns exec ns-test ./teardown-in-ns.sh
    sudo ip netns delete ns-test
}

# These should all be run in the same namespace
function config-in-ns {
    ip addr add 10.0.0.2/24 dev veb-test
    ip link set veb-test up
    ip link set dev lo up
    ip tuntap add tap-test mode tap user rett
    ip link set tap-test up
}

# These should all be run in the same namespace
function teardown-in-ns {
    ip tuntap delete veb-test mode tap
    ip link set dev veb-test down
    ip link delete dev veb-test
    ip link set dev lo down
}
