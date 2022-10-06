import std/[posix, posix_utils]
import os
proc run() {.async.} =
  while true:
    let pid = fork()
    if pid == 0: # sub process
      # stuff run in subprocess
      quit()
    else:
      # monitor forked process...
      var status: cint
      while true:
        try:
          sendSignal(pid, 0)
          discard waitpid(pid, status, WNOHANG)
          sleep(1000)
        except OSError as e:
          warn "Child process terminated. Restarting."
          break
