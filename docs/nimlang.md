
  * `nimls` doesn't find `#master` tags. So packages have to be renamed with a new tag.
  ```nim
  MYPKG=weave
  VERSION=0.4.1
  cd ~/.nimble/pkgs
  mv "$MYPKG-"* "$MYPKG-0.4.1"
  ```
   * Make sure there is only one package version installed (in `~/.nimble/pkgs`) if running compile commands from nim, eg: `nim r -r ...mynimfile.nim`. Because there isn't a way to specify the version of an imported module without using nimble which uses it configuration, (and it doesn't look like the imported module is the _latest_ either by version (it's not master) or by install date).
