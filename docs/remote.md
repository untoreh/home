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

## Wezterm multiplexing
To enable wezterm multiplexer check that:
- The remote server has the mux server binary. Otherwise add it from edge repo.
``` sh
# on remote
apkc /opt/alp add wezterm-mux-server@edge
```
- Since locally wezterm starts from windows, it has to lookup correct ssh config file from the windows user home folder, e.g. `c:\Users\$USER\.ssh`. Drop a symbolic link from the *realpath* of the ssh config file using the [link shell extension](https://web.archive.org/web/20230103222429/https://schinagl.priv.at/nt/hardlinkshellext/linkshellextension.html). If the ssh config specifiy an identity file, the path of such identity file will also have to be linked.
- The remote also has to have a `~/.wezterm.lua` file at least to specify the correct color scheme:

``` lua
local wezterm = require 'wezterm';

return {
  color_scheme = "Dracula",
}
```
- The local config has to have parsed the ssh config, from wezterm documentation it shows how to do it using `wezterm.enumerate_ssh_hosts()`.
- if the wezterm is not available in the remote (e.g. error `127`) make sure the ssh proxy command is spawned with correct `$PATH` that includes the location of the wezterm-mux-server. (otherwise in alpine remote add `command="exec /opt/bin/ssheval"` to the pub key in `~/.ssh/authorized_keys`)
- Finally to connect to the remote host run this from a windows shell, or powertoys `> ...`:

``` sh
wezterm connect myhostname
```

