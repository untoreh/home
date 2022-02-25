# Remote servers troubleshooting

## Suspiciously unresponsive requests

Check IPv6
If the server (container) has an ipv6 IP but ipv6 routing doesn't (seem to) work try to disable ipv6:

`sysctl -p`...
``` sh
net.ipv6.conf.all.disable_ipv6 = 1
net.ipv6.conf.default.disable_ipv6 = 1
```

When you try to ping a remote server, check the resolved IP if it is ipv4 or ipv6. 
Modifying the `/etc/gai.conf` didn't work in my case..
