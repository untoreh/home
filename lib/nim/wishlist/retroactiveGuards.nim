import std/macros

var glock {.compileTime.} = false
var doGuard {.compileTime.} = false

macro checkLocks(code: typed) = # must be typed to catch lock calls at top level
  if not glock and doGuard:
    error("Lock was required but not held.")
  glock = false
  doGuard = false
  result = code

macro lock(code: untyped) =
  if not doGuard:
    error("Tried to lock without guard request.")
  if glock:
    error("Lock was already set.")
  else:
    glock = true
  result = code
  glock = false

macro guarded(code: untyped) =
  if doGuard:
    error("Guard was already set.")
  else:
    doGuard = true
  result = code

proc a(): int =
  guarded():
    return 1 + 1

proc aa(): int =
  lock:  # (un)comment me
    return a()

proc pls() =
  guarded():
    echo "yes"

proc aaa() {.checkLocks.} =
  lock: # (un)comment me
    echo aa()
  pls()


aaa()
