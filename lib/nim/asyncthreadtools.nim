import chronos
import chronos/timer
import std/locks

type
  AsyncThreadLockObj = object
    lock: Lock
  AsyncThreadLock = ptr AsyncThreadLockObj

proc acquire*(l: AsyncThreadLock): Future[void] {.async.} =
  while not l.lock.tryAcquire():
    await sleepAsync(100.nanoseconds)

proc release*(l: AsyncThreadLock) =
  l.lock.release()

import threading/atomics
## In this implementation it is the waiters jobs to clear the fire flag
type
  AsyncThreadEventObj = object
    fired: Atomic[bool]
    waiters: Atomic[uint]
  AsyncThreadEvent* = ptr AsyncThreadEventObj

proc fire*(e: AsyncThreadEvent) =
  e.fired.store(true)

proc wait*(e: AsyncThreadEvent) {.async.} =
  if not e.fired.load:
    e.waiters.inc
    while true:
      if e.fired.load:
        e.waiters.dec
        if e.waiters.load == 0:
          e.fired.store(false)
        break
      else:
        await sleepAsync(100.nanoseconds)
