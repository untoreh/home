## Gpg error: "no default key"
Key is most likely expired.

## Update expiring key
Use `sudo` otherwise you can't save the modifications.

``` sh
se gpg --edit-key $FINGERPRINT
```

- Type `expire`
- Insert the duration, eg `5y` for five years.
- Type `save` to save modifications and quit

Make sure devices are synchronized.

## Inappropriate ioctl...
when calling a command like `gpg -d ...` the user agent daemon might be started automatically, but it will be started with the environment from where the command was spawned from. This starts the agent with the wrong config. Instead ensure that the `gpg-user-agent` systemd unit is started before issuing any command to ensure that it is started with the (correct) systemd environment.

