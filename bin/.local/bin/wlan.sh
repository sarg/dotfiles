#!/bin/bash

wpa_cli="wpa_cli"

if [ -z "$1" ]; then
    $wpa_cli status
elif [ "$1" == "c" ]; then
    if [ -z "$2" ]; then
       $wpa_cli
    else
        $wpa_cli select_network $2
    fi
elif [ "$1" == "l" ]; then
    $wpa_cli list_networks
elif [ "$1" == "d" ]; then
    $wpa_cli disconnect
elif [ "$1" == "r" ]; then
    $wpa_cli reassociate
elif [ "$1" == "s" ]; then
    $wpa_cli scan
    # wpa_block_scan /var/run/wpa_supplicant/wlan0
    $wpa_cli scan_results
elif [ "$1" == "t" ]; then
    if /usr/sbin/rfkill list wlan | grep -qP Soft.+yes; then
        /usr/sbin/rfkill unblock wlan
        echo "Enabled"
    else
        /usr/sbin/rfkill block wlan
        echo "Disabled"
    fi
elif [ "$1" == "e" ]; then
    $wpa_cli list_networks | tail -n+2 | awk '{print $1}' | xargs -n1 $wpa_cli enable_network
elif [ "$1" == "w" ]; then
    { $wpa_cli status; ip a; } | perl -e 'use strict; my $s; while (<>) { if (/wpa_state=(.)/) { $s = $1; } elsif (/scope global/) { $s = "U" } } print "$s\n"'
elif [ -z "$2" ]; then
    id=$($wpa_cli add_network | tail -n1)
    $wpa_cli set_network $id key_mgmt NONE
    $wpa_cli set_network $id ssid "\"$1\""
    $wpa_cli enable_network $id
else
    id=$($wpa_cli add_network | tail -n1)
    $wpa_cli set_network $id key_mgmt WPA-PSK
    $wpa_cli set_network $id proto RSN
    $wpa_cli set_network $id group CCMP TKIP
    $wpa_cli set_network $id ssid "\"$1\""
    $wpa_cli set_network $id psk "\"$2\""
    $wpa_cli enable_network $id
fi
