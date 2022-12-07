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

## Weird ssh/sctp hang ups
Using dropbear server with a too high `recv_window_size` will cause `openssh` to terminate the connection.
This is because by the max recv window is very close to the default value. Effectively dropbear upstream compile time constants allow the `-W` command line flag to only __decrease__ the recv window. If the window is set too high at runtime, while dropbear is compiled with `RECV_MAX_PAYLOAD_LEN` below such value, dropbear WILL hang up connections. Solutions:
- Remove the `-W` flag from dropbear runtime config
- compile dropbear with an high enough `RECV_MAX_PAYLOAD_LEN`
- Use a small enough buffer the `sftp` (like `-B 512`)

## Windows port forwarding (WSL from lan) not working
- Check that the service `IP helper` is running.

## Wezterm + Tmux spews garbage
If attaching to a tmux session when using wezterm terminal prints a bunch of characters in the shell, set tmux `escape-time` to 1 (not 0)
