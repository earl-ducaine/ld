# https://superuser.com/questions/764986/howto-setup-a-veth-virtual-network/765078

sudo ip link add dev vm1 type veth peer name vm2
sudo ip link set dev vm1 up
sudo ip tuntap add ldtap mode tap
sudo ip link set dev ldtap up
sudo ip link add brm type bridge

sudo ip link set ldtap master brm
sudo ip link set vm1 master brm

sudo ip addr add 10.0.0.1/24 dev brm
sudo ip addr add 10.0.0.2/24 dev vm2

sudo ip link set brm up
sudo ip link set vm2 up

sniff(iface="wifi0", prn=lambda x: x.summary())
