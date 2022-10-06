import chronos
import chronos/timer
import std/locks

type
  AsyncThreadLockObj = object
    lock: Lock
  AsyncThreadLock = ptr AsyncThreadLockObj

template withLock(lock: AsyncThreadLock, code): untyped =
  await lock.acquire()
  try:
    code
  finally:
    lock.release()

proc acquire*(l: AsyncThreadLock): Future[void] {.async.} =
  while not l.lock.tryAcquire():
    await sleepAsync(100.nanoseconds)

proc release*(l: AsyncThreadLock) =
  l.lock.release()

import threading/atomics
## This implementation fire is instant, but spawns an async function
## that waits for all waiters to clear out, fire can overlap
type
  AsyncThreadEventObj = object
    lock: AsyncThreadLock
    fired: Atomic[bool]
    waiters: Atomic[uint]
  AsyncThreadEvent* = ptr AsyncThreadEventObj

proc init*(_: typedesc[AsyncThreadEvent]): AsyncThreadEvent =
  result = create(AsyncThreadEventObj)
  result.lock = create(AsyncThreadLockObj)

proc fireImpl(e: AsyncThreadEvent) {.async.} =
  try:
    e.fired.store(true)
    while true:
      if e.waiters.load == 0:
        break
      else:
        await sleepAsync(100.nanoseconds)
  finally:
    e.fired.store(false)

proc fire*(e: AsyncThreadEvent) =
  asyncSpawn fireImpl(e)

proc wait*(e: AsyncThreadEvent) {.async.} =
  if not e.fired.load:
    e.waiters.inc
    while true:
      if e.fired.load:
        e.waiters.dec
        break
      else:
        await sleepAsync(100.nanoseconds)
