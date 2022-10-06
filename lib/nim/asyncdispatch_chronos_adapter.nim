import chronos
import asyncdispatch except async, multisync, await, waitFor, Future,
    FutureBase, asyncSpawn, sleepAsync

const DEFAULT_TIMEOUT = 6.seconds

proc waitLoop[T](fut: asyncdispatch.Future[T]) {.async.} =
  while not fut.finished():
    await sleepAsync(1.milliseconds)

proc wait*(fut: asyncdispatch.Future[void],
    timeout = DEFAULT_TIMEOUT) {.async.} =
  try:
    await waitLoop(fut).wait(timeout)
  except AsyncTimeoutError as e:
    fail(fut, e)
    raise e

proc wait*[T: not void](fut: asyncdispatch.Future[T],
    timeout = DEFAULT_TIMEOUT): Future[T] {.async.} =
  try:
    await waitLoop(fut).wait(timeout)
    result = fut.read()
  except AsyncTimeoutError as e:
    fail(fut, e)
    raise e
