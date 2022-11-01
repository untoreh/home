import std/[os, monotimes, httpcore, uri, asyncnet, net,
            hashes, locks, strutils]
import asyncdispatch as ad except async, Future, FutureBase, sleepAsync
import chronos as ch
import chronos/timer
import httptypes
import utils
import ./harpoon
from cfg import PROXY_EP

const DEFAULT_TIMEOUT = 4.seconds

proc waitLoop[T](fut: ad.Future[T]) {.async.} =
  while not ad.finished(fut):
    await sleepAsync(1.milliseconds)

proc wait*(fut: ad.Future[void], timeout = DEFAULT_TIMEOUT) {.async.} =
  await waitLoop(fut).wait(timeout)

proc wait*[T: not void](fut: ad.Future[T], timeout = DEFAULT_TIMEOUT): Future[T] {.async.} =
  await waitLoop(fut).wait(timeout)
  result = ad.read(fut)

var httpThread: Thread[void]

# proc hash(rq: ptr Request): Hash = hash(rq.url)

import nimSocks/[types, client]

const PROXY_HOST = parseUri(PROXY_EP).hostname
const PROXY_PORT = parseUri(PROXY_EP).port.parseInt.Port
const PROXY_METHODS = {NO_AUTHENTICATION_REQUIRED, USERNAME_PASSWORD}

proc getPort(url: Uri): Port =
  case url.scheme:
    of "http": Port 80
    of "https": Port 443
    else: Port 80


proc getConn(url: Uri, port: Port, proxied: bool): Future[
    asyncnet.AsyncSocket] {.async.} =
  var sock: asyncnet.AsyncSocket
  try:
    if proxied:
      sock = await asyncnet.dial(PROXY_HOST, PROXY_PORT).wait()
      if not proxied:
        return sock
      if not await sock.doSocksHandshake(methods = PROXY_METHODS).wait():
        sock.close
        raise newException(OSError, "Proxy error.")
      if not await sock.doSocksConnect(url.hostname, port).wait():
        sock.close
        raise newException(OSError, "Proxy error.")
    else:
      sock = newAsyncSocket()
    return sock
  except Exception as e:
    if not sock.isnil:
      sock.close()
    raise e

converter toSeq(headers: HttpHeaders): seq[(string, string)] =
  if not headers.isnil:
    for (k, v) in headers.pairs():
      result.add (k, v)

converter toHeaders(s: seq[(string, string)]): HttpHeaders = s.newHttpHeaders()

# import std/importutils
# privateAccess(AsyncTable)
# ch.async:
#   proc put*[K, V](t: AsyncTable[K, V], k: K, v: V) =
#     try:
#       ch.await t.lock.acquire()
#       if k in t.waiters[]:
#         var ws: ptr seq[ptr Future[V]]
#         doassert t.waiters[].pop(k, ws)
#         defer: dealloc(ws)
#         while ws[].len > 0:
#           let w = ws[].pop()
#           if not w.isnil:
#             w[].complete(v)
#       else:
#         t.table[][k] = v
#     finally:
#       t.lock.release

proc doReq(rq: ptr Request, timeout = 6000) {.async.} =
  let r = newResponse()
  # defer: maybefree(r)
  let e = newException(RequestError, "Bad code.")
  var conn: AsyncSocket
  try:
    let port = rq.url.getPort()
    conn = await getConn(rq.url, port, rq.proxied)
    let resp = await fetch(conn,
                          $rq.url,
                          metod = rq.meth,
                          headers = rq.headers,
                          body = rq.body,
                          timeout = timeout,
                          skipConnect = rq.proxied,
                          port = port,
                          portSsl = port
      ).wait()
    r.code = resp.code
    r.headers[] = resp.headers.toHeaders()
    r.body[] = resp.body
  except CatchableError as e: # timeout?
    echo e[].msg
    discard
  finally:
    if not conn.isnil:
      conn.close()
    # the response
    ch.await httpOut.put(rq, r)

proc httpHandler() =
  var rq: ptr Request
  while true:
    try:
      warn "http: starting httpHandler..."
      while true:
        rq = ch.waitFor httpIn.pop
        checkNil(rq):
          ch.asyncSpawn doReq(rq)
        # for debugging
        # let r = newResponse()
        # ch.waitFor httpOut.put(rq, r)
    except:
      let e = getCurrentException()
      warn "http: httpHandler crashed. {e[].msg}"

proc initHttp*() =
  httpTypes.initHttp()
  createThread(httpThread, httpHandler)
