* MDBX_CORRUPTED
You get a corrupted mdbx database
- MDBX has a tool called `mdbx_chk` which you can use to try to repair the db. Find it in the `libmdbx` pkg of your distro or build from source from the libmdbx version (which you might have to do anyway to match the correct tooling version to your version of the database.):

``` sh
mdbx_chk -w $DBPATH
```

Most likely it will fail.

- To recover what you can from the database make a dump and load it in a new database.

``` sh
mdbx_dump -a -r $DBPATH -f dump.raw
mdbx_load -r -f dump.raw $NEWDBPATH
mdbx_chk $NEWDBPATH/mdbx.dat
```
