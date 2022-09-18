import vendor/chronos/chronos, os, uri
# import std/asyncdispatch
import stew/results
import vendor/chronos/chronos/transports/stream
import vendor/chronos/chronos/asyncloop
import vendor/chronos/chronos/apps/http/[httpclient {.all.}, httpcommon]


# GET http://httpbin.org/get HTTP/1.1
# Host: httpbin.org
# Connection: Keep-Alive
# user-agent: Nim httpclient/1.6.7
# var fd: AsyncFD
# register(fd)
# let tp = newStreamSocketTransport(fd)
let sess = new(HttpSessionRef,
               connectTimeout=2.seconds,
               headersTimeout=4.seconds,
               proxy="http://localhost:8877",
               proxyAuth=proxyAuth("hello", "wow"))
# let u = parseUri("http://localhost:8877")
# let u = parseUri("http://httpbin.org/get")
# sess.proxy = getAddress(sess, u).get
# let conn = waitFor sess.acquireConnection(ha)
# # waitFor conn.writer.write("CONNECT reqbin.com:443 HTTP/1.1")
# # echo waitFor conn.transp.readLine()
# # echo waitFor conn.transp.readLine()
let ha = getAddress(sess, "http://httpbin.org/get").get
let req = get(HttpClientRequestRef, sess, ha)
let resp = waitFor req.send()
echo resp.status
echo bytesToString (waitFor resp.getBodyBytes())
