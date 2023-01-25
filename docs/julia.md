# Quirks

## PyCall
- can fail to build if some packages mess with `Conda`. E.g. pycall calls `Conda.add("numpy")` and stalls indefinitely because some packages are causing conflicts and conda keeps trying to _solve_ the environment. You want to remove offending packages likes `Conda.pip("uninstall", ...)`

## Franklin or other markdown compilation errors in emacs
- set `markdown-spaces-after-code-fence` to `0` because spaces between triple quotes and the lang keyword can cause compile errors.
