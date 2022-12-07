import chronos
import httptypes

## NOTE: probably broken?

import os
var t: Thread[ptr int]

type
  AsyncChannelObj[T] = object
    chan: Channel[T]
  AsyncChannel*[T] = ptr AsyncChannelObj[T]

template ensure(code) =
  var c = 10.milliseconds
  while not code:
    await sleepAsync(c)
    c += c

proc init*[T](_: typedesc[AsyncChannel[T]]): AsyncChannel[T] =
  result = create(AsyncChannelObj[T])
  open(result.chan)

proc send*[T](c: AsyncChannel[T], v: T) {.async.} =
  ensure:
    c.chan.trysend(v)

template put*[T](c: AsyncChannel[T], v: T) = await c.send(v)
template add*[T](c: AsyncChannel[T], v: T) = await c.send(v)

proc recv*[T](c: AsyncChannel[T]): Future[T] {.async.} =
  var ret: bool
  ensure:
    (ret, result) = c.chan.tryrecv()
    ret

template pop*[T](c: AsyncChannel[T], v: var T) = v = await c.recv()
