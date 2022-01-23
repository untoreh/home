# WSL

We try to have a 1:1 correspondency between a WSL user config and a bare Linux user config in order to switch between native and virt seamlessly.
This is somewhat hard because WSL doesn't directly support systemd and workarounds like `genie` and `subssystemctl` are not inspiring.
Because of this we choose to use a user space process manager, [`supervisor`](https://github.com/Supervisor/supervisor). (a close second was GNU pies)
Thankfully, (and consciously) we don't manage a lot of userspace services. Here's a list of services used, although not all of them are currently enabled.
_services_ are _disabled_

# apps

- emacs
- firefox
- borgbackup
- _mediabox-backup_
- _trakt-scrobbler_
- _wallpaper_

# system

- kwin-restart
- _samba_
- mounts
- sys-update (ubuntu)
- _win10_
- zswap

# sway

- _mako_
- _swayidle_
- _polkit_
- _pulseaudio_
- _audioswitch_
- _xsettingsd_

## Services for WSL

Since lots of them are disabled (at time of writing) we only have to port a subset of services to `supervisor` to make them work under WSL. Moreover because a custom kernel would be required for zswap, we forfeit it for now, also because it is unclear if there are benefits running zswap since windows has its own memory compression strategy. Because we don't have a KDE session, we don't need `kwin-restart` (that was a workaound kwin Video memory leaks when opening new apps). Also we use Arch under WSL and not ubuntu, therefore sys-update needs a different wsl compatible version. Firefox is also pointless to autostart on Windows, and WSL has its own config for mounting, (also because it doesn't support custom filesystems.)
This leaves us we a pretty short list of services to write supervisor configs for:

- emacs
- firefox (maybe since it is run under windows..)
- borgbackup (this requires WSL paths support)
- dhclient (because we give our linux VM a dedicated LAN IP)
- syncthing
- sshd


So...only three for now...

## emacs
### libvterm
Don't use system libvterm
```sh
cd ~/.emacs.d/.local/straight/build*/vterm/build
cmake -DUSE_SYSTEM_LIBVTERM=no ..
make
```

## clipboard
With neovim, set `clipboard=unnamedplus`, and make sure to **not** have `wl-clipboard` or `xcip` installed, as neovim will prefer those over `w32yank` and won't work correctly.

## Networking
To allow seamless access to services listening on IPv4 from within WSL without tunneling, change the Hyper-V virtual switch for WSL to be external (the actual hardware, e.g. RealTek), and shared with the HOST machine.

## WSL Path slows down shell completion
This seems a problem shared among all linux shells, having the windows path appended makes autocomplete really slow, so we configure wsl not to append the windows path. Use the wrapper `~/bin/wex` to call windows executables from wsl instead, i.e. `wex powershell.exe`.

## /etc/wsl.conf

```conf
[automount]
enabled=true
mountFsTab=true
[Interop]
appendWindowsPath = False
```

## WSL doesn't work because virtualization is not enabled
On AMD platforms the options to enable in the BIOS are `SVM` and `NX mode`.

## Mount /home and sshd
If can't ssh into home directory, check permissions of `/home/$USER`

``` sh
sudo chmod 755 $HOME
```

# Not WSL related

## Export/Import env variables

```
## export
SET >> allvariables.txt
## import
for /F %A in (allvariables.txt) do SET %A
```


## Hibernation

``` sh
powercfg /h /type full
```

## Bluetooth
I experienced bluetooth stuttering between play/resume media. The only way that I was able to fix this was by installing the CSR bluetooth harmony stack, which can be googled, usually shipped with a cd for bluetooth dongles. This stack replaces entirely all the bluetooth services that comes with Windows, seems more featureful.

## Clear Windows Store Cache

``` sh
WSRESET
```

