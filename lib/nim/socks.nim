import std/[asyncdispatch, asyncnet, uri, strutils, strformat, httpclient]
import nimSocks/[types, client]
import utils

const defaultServer = parseUri("socks5://127.0.0.1:8877")

func getPort(u: Uri, default = 80): Port =
  if u.port != "":
    Port u.port.parseInt
  elif default != 80:
    Port default
  elif u.scheme == "https":
    Port 443
  else:
    Port default

proc sockConnect(target: Uri, server = defaultServer): Future[
    AsyncSocket] {.async.} =
  checkTrue server.hostname != "", "socks5: server hostname is empty"
  var sock = await asyncnet.dial(server.hostname, server.getPort(
      1080)) # dial to the socks server
  if not await sock.doSocksHandshake(
      # username="username",
        # password="password",
    methods = {NO_AUTHENTICATION_REQUIRED,
        USERNAME_PASSWORD} # the "best" auth supported gets choosen by the server!
    ):
    sock.close()
  if not await sock.doSocksConnect(target.hostname,
                                   target.getPort): # instruct the proxy to connect to target host (by tcp)
    sock.close()
  return sock

template sockConnect(target: string, server = defaultServer): untyped =
    var uri = target.parseUri
    if uri.hostname == "" and target != "":
      uri = parseUri("//" & target)
    sockConnect(uri, server)
