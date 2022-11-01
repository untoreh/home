import sharedqueue
import asyncdispatch
import locks
type
  AsyncPCollObj[T] = object
    lock: Lock
    waiters: PColl[ptr Future[T]]
    pcoll: Pcoll[T]
  AsyncPColl*[T] = ptr AsyncPCollObj[T]

proc newAsyncPColl*[T](): AsyncPColl[T] =
  result = create(AsyncPCollObj[T])
  initLock(result.lock)
  result.pcoll = newColl[T]()
  result.waiters = newColl[ptr Future[T]]()
  assert not result.waiters.isnil

proc add*[T](apc: AsyncPColl[T], v: T) =
  withLock(apc.lock):

    if apc.waiters.len > 0:
      apc.waiters[0][].complete(v)
      apc.waiters.delete(0)
    else:
      apc.pcoll.add v

proc newFuturePtr[T](name: static string): ptr Future[T] =
  let fut = create(Future[T])
  fut[] = newFuture[T](name)
  proc cb() =
    if not fut.isnil:
      reset(fut[])
      dealloc(fut)
  addCallBack(fut[], cb)
  fut


proc pop*[T](apc: AsyncPColl[T]): Future[T] =
  let fut = newFuturePtr[T]("AsyncPColl.pop")
  withLock(apc.lock):
    var v: T
    if apc.pcoll.pop(v):
      fut[].complete(v)
      return fut[]
    else:
      apc.waiters.add fut
  fut[]

proc add*[T](apc: AsyncPColl[T]): Future[T] {.async.}  =
  withLock(apc.lock):
    var clear: seq[int]
    for i in 0..<apc.waiters.len:
      if apc.waiters[i].finished:
        clear.add i
    apc.waiters.pop(clear)
  result = await apc.popImpl()
  # withLock(apc.lock):
  #   apc.waiters[0].delete()

proc delete*[T](apc: AsyncPColl[T]) =
  apc.waiters.delete()
  apc.pcoll.delete()
  deinitLock(apc.lock)
  dealloc(apc)

type
  ThreadLockObj = object
    lock: Lock
    waiters: PColl[ptr Future[void]]
  ThreadLock = ptr ThreadLockObj

proc newThreadLock*(): ThreadLock =
  result = create(ThreadLockObj)
  initLock(result.lock)
  result.waiters = newColl[ptr Future[void]]()

proc acquire*(t: ThreadLock): Future[void] {.async.} =
  let fut = newFuturePtr[void]("ThreadLock.acquire")
  if t.lock.tryacquire():
    fut[].complete()
  else:
    t.waiters.add fut
    await fut[]
    doassert t.lock.tryacquire

proc release*(t: ThreadLock) =
  if unlikely(t.lock.tryAcquire):
    t.lock.release
    raise newException(ValueError, "ThreadLock was unlocked.")
  t.lock.release
  if t.waiters.len > 0:
    t.waiters[0][].complete()
    t.waiters.delete(0)

template withLock*(l: ThreadLock, code): untyped =
  try:
    await l.acquire()
    code
  finally:
    l.release()
