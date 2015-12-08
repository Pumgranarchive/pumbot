#!/bin/bash

iptables -t nat -A PREROUTING -i eth0 -p tcp --dport http -j REDIRECT --to-port 8083
