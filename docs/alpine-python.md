## Using pip with alpine
Since many packages don't ship alpine compatible builds, they have to be built locally. Common build dependencies as alpine packages:

``` sh
apkc /opt/alp add alpine-sdk python3-dev libffi-dev # py3-wheel
```

If these are being installed inside a non root fs, e.g. `/opt/alp` then you need to switch root (to avoid messing with env vars):
`jail alp --bind /opt/py /opt/py /bin/sh` if using `pine`, and then
load the python environment:
`cd /opt/py; . .env/bin/activate`

`wheel` can also be installed from `pip`:
`pip install wheel`

# Force flag
Don't use the force flag, as that will also force install dependencies, (which we most likely want to avoid and use distro built packages). Instead uninstall the package, and reinstall it.
