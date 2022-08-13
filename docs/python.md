# Python troubleshooting

## The jupyter console doesn't work, throws stacktraces
Incompatible python and jupyter versions.
- Remove `loop=loop` from line 664 in `jupyter_console/ptshell.py`
- `pip install jupyter-client==6.1.12`
- `pip install readline`

## Module not found errors
Some version of Broken Environment Paths:
- you installed a virtual env and dependencies, but then switched python version
- you are using python from nix, and a virtual env at the same time, if you use nix, restrict to nix deps.
- you cache the generation of the `PYTHONPATH` variable, it got stale and is pointing to an incorrect version of `../site-packages`
- you installed packages with python from cli, but are using python in another program from `libpython` and you are loading the wrong version of libpython. It has to match what you used to install dependencies, better avoid stalling dependencies from cli and drive pip from libpython.
