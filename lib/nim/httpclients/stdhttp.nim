import std/[os, monotimes, httpcore, uri, httpclient, net, hashes, locks]
import chronos/timer
import asyncdispatch except `$` # overlaps with system `$`
from asyncfutures import asyncCheck
import httptypes as htp
export htp
import sharedqueue
import utils
from cfg import PROXY_EP

const DEFAULT_TIMEOUT = 4.seconds # 4 seconds

type
  Request = htp.Request
  Response = htp.Response

var
  httpThread: Thread[void]

proc hash(rq: ptr Request): Hash = hash(rq.url)

proc wait[T](fut: Future[T], timeout: Duration): Future[T] {.async.} =
  let start = Moment.now()
  while true:
    if fut.finished():
      result = fut.read()
      break
    elif Moment.now() - start > timeout:
      raise newException(TimeoutError, "Timeout exceeded!")
    else:
      await sleepAsync(10)


proc getClient(redir=true): AsyncHttpClient =
  newAsyncHttpClient(
    maxRedirects=(if redir: 5 else: 0),
    proxy=newProxy(PROXY_EP),
    sslContext=newContext(verifyMode = CVerifyNone)
  )

proc doReq(rq: ptr Request, timeout = DEFAULT_TIMEOUT) {.async.} =
  let r = newResponse()
  defer: maybeFree(r)
  var cl: AsyncHttpClient
  try:
    cl = getClient(rq.redir)
    let resp = await cl.request(rq.url, httpMethod = rq.meth,
        headers = rq.headers, body = rq.body).wait(timeout)
    r.code = resp.code
    r.headers[] = resp.headers
    if r.code == Http200:
      r.body = create(string)
      try:
        r.body[] = await resp.body
      except Exception as e:
        free(r.body)
        raise e
  except ProtocolError as e: # timeout?
    warn "protocol error: is proxy running? {PROXY_EP}"
    await sleepAsync(1000)
  except CatchableError:
    discard
  finally:
    if not cl.isnil:
      cl.close()
  # the response
  httpOut[rq] = r

proc popFirstAsync[T](q: PColl[T]): Future[T] {.async.} =
  while true:
    if q.len > 0:
      doassert q.pop(result)
      break
    else:
      await sleepAsync(1)

proc asyncHttpHandler() {.async.} =
  var rq: ptr Request
  while true:
    try:
      warn "http: starting httpHandler..."
      while true:
        rq = await httpIn.popFirstAsync
        checkNil(rq):
          asyncCheck doReq(rq)
    except:
      let e = getCurrentException()
      warn "http: httpHandler crashed. {e[]}"

proc httpHandler() =
  waitFor asyncHttpHandler()

proc initHttp*() =
  notNil(httpIn):
    delete(httpIn)
  httpIn =  newColl[ptr Request]()
  setNil(httpOut):
    initLockTable[ptr Request, ptr Response]()
  createThread(httpThread, httpHandler)
