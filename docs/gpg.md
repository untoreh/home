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



