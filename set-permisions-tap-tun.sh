#!/bin/bash #-*- mode: shell-script;  -*-

function set-permissions {
    echo 'KERNEL=="tun", GROUP="netdev", MODE="0660",'\
    	 'OPTIONS+="static_node=net/tun"' >> \
	 "/etc/udev/rules.d/zzz_net_tun.rules"
}
