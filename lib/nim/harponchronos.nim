import std/[strutils, httpcore]
import chronos
import nimSocks/[client, types]
import harpoon
import uri

# let taddr = initAddress

# dial to the socks server
var sock = waitFor asyncnet.dial("127.0.0.1", Port 8877)
assert true == waitFor sock.doSocksHandshake(
  username = "username",
  password = "password",
  # the "best" auth supported gets choosen by the server!
  methods = {NO_AUTHENTICATION_REQUIRED, USERNAME_PASSWORD}
  )
let u = "https://httpbin.org/get".parseUri
let port = Port (if u.port != "": u.port.parseInt elif u.scheme ==
    "https": 443 else: 80)
assert true == waitFor sock.doSocksConnect(u.hostname, port) # i

let headers: seq[(string, string)] = @[]
let resp = waitfor fetch(sock, $u, HttpGet, headers, timeout = 3000, skipConnect = true)
if resp.code == HttpCode(0):
  raise newException(IOError, "Timeout!")
else:
  echo resp.body
