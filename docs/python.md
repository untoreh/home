# Python troubleshooting

## The jupyter console doesn't work, throws stacktraces
Incompatible python and jupyter versions.
- Remove `loop=loop` from line 664 in `jupyter_console/ptshell.py`
- `pip install jupyter-client==6.1.12`
- `pip install readline`
