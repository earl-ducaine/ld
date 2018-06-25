# Ethnet network device (that sits on the ethernet bridge: ldtap
sniff(iface="eno1", prn=lambda x: x.summary())

sr(IP(dst="192.168.11.1")/TCP(sport=RandShort(),dport=[440,441,442,443], flags="S"))

ans.summary( lambda(s,r): r.sprintf("%TCP.sport% \t %TCP.flags%") )





# Setting up Ethernet bridge

function setup-ethernet-bridge {
    # Surely we don't want to flush our good interface?
    # ip addr flush dev inet
    ip addr flush dev ldtap
    sudo brctl addbr ldbridge
    sudo brctl addif ldbridge ldtap eno1
    sudo ip link set dev ldbridge up
}


# eno1: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
#         inet 192.168.11.102  netmask 255.255.255.0  broadcast 192.168.11.255
#         inet6 fe80::21f:c6ff:fe9b:aa5f  prefixlen 64  scopeid 0x20<link>
#         ether 00:1f:c6:9b:aa:5f  txqueuelen 1000  (Ethernet)
#         RX packets 589908  bytes 763945128 (763.9 MB)
#         RX errors 0  dropped 0  overruns 0  frame 0
#         TX packets 876850  bytes 1080282764 (1.0 GB)
#         TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0
#         device interrupt 16  memory 0xdc200000-dc220000


# To run Scapy with just cap_net_raw privilege...
# The safest and less complicated way I know is, in order:
#     Make a personal copy of the python binary:
#     $ sudo cp /usr/bin/python2.7 ~/python_netraw
#     Own it:
#     $ sudo chown your user name ~/python_netraw
#     Don't let anybody else run it:
#     $ chmod -x,u+x ~/python_netraw
#     Give it cap_net_raw capability:
#     $ sudo setcap cap_net_raw=eip /usr/bin/python_netraw
#     Run scapy with it:
#     $ ~/python_netraw -O /usr/bin/scapy
# (Or use sudo each time you need to run Scapy with raw privileges.)
