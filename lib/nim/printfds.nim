import os, strformat
proc fdn(n: int) =
    discard execShellCmd(fmt"echo -n 'FDs {n}: ' ; ls /proc/{getCurrentProcessId()}/fd | wc -l")
