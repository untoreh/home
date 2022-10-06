import std/[os, exitprocs, strutils, sequtils],
       asynctools/asyncipc,
       chronos,
       strformat
import asyncdispatch except async, await, waitFor, Future, FutureBase, asyncSpawn, sleepAsync

const
  sleepTime = 100.nanoseconds
  timeout = 1.seconds
  bufferSize = 1024 * 4
  maxProcessMem = 1024 * 1024 * 1024
var
  # Pipe and lock for to be translated text
  inputLock: ptr AsyncLock  # consumer is synchronous
  inputRecvIpc: ptr AsyncIpcHandle
  inputSendIpc: ptr AsyncIpcHandle
  # Pipe and lock for translated text
  outputLock: ptr AsyncLock # forwarder is synchronous
  outputSendIpc: ptr AsyncIpcHandle
  outputRecvIpc: ptr AsyncIpcHandle

const ipcNameIn = "transInput"
const ipcNameOut = "transOuput"
const ipcSize = 10 * 1024 * 1024

proc toString*(bytes: openarray[byte | char]): string =
  result = newString(bytes.len)
  copyMem(result[0].addr, bytes[0].unsafeAddr, bytes.len)

template setNil*(id, val) =
  if id.isnil:
    id = val

template ifNil*(id, val) =
  if id.isnil:
    val

template maybeCreate*(id, tp; force: static[bool] = false) =
  when force:
    id = create(tp)
  else:
    if id.isnil:
      id = create(tp)
  reset(id[])

proc setupIpc(name: string, force = false) =
  try:
    let ipc =
      try:
        createIpc(name, ipcSize)
      except OSError:
        if force:
          let path = getTempDir() / ("asyncipc_" & name)
          if not path.tryRemoveFile:
            raise newException(OSError, fmt"Couldn't create ipc bus {name}.")
          createIpc(name)
        else:
          raise newException(OSError, "Ipc file exists.")
    proc closeIpc() =
      close(ipc)
    {.cast(gcsafe).}:
      addExitProc(closeIpc)
  except OSError:
    discard

template connectSendIpc(prefix, name) =
  `prefix SendIpc`.maybeCreate(AsyncIpcHandle, false)
  `prefix SendIpc`[] = open(name, sideWriter, register=true)

template connectRecvIpc(prefix, name) =
  `prefix RecvIpc`.maybeCreate(AsyncIpcHandle, false)
  `prefix RecvIpc`[] = open(name, sideReader, register=true)

proc initIpc*() {.gcsafe.} =
  inputLock = create(AsyncLock)
  inputLock[] = newAsyncLock()

  outputLock = create(AsyncLock)
  outputLock[] = newAsyncLock()

  setupIpc(ipcNameIn)
  setupIpc(ipcNameOut)

  connectRecvIpc(input, ipcNameIn) # translator recieve text to be translated
  connectSendIpc(input, ipcNameIn) # client sends text to be translated

  connectRecvIpc(output, ipcNameOut) # client receives translated text
  connectSendIpc(output, ipcNameOut) # server sends translated text

proc waitLoop[T](fut: asyncdispatch.Future[T]) {.async.} =
  while not fut.finished():
    await sleepAsync(sleepTime)

proc wait(fut: asyncdispatch.Future[void], timeout = timeout) {.async.} =
  await waitLoop(fut).wait(timeout)

proc wait[T: not void](fut: asyncdispatch.Future[T], timeout = timeout): Future[T] {.async.} =
  await waitLoop(fut).wait(timeout)
  result = fut.read()

proc write(output: AsyncIpcHandle, s: string) {.async.} =
  if s.len == 0:
    return
  let header = s.len
  await output.write(header.unsafeAddr, sizeof(header)).wait
  await output.write(s[0].unsafeAddr, s.len).wait

proc read(input: AsyncIpcHandle, n: static int): Future[string] {.async.} =
  mixin toString
  var dst: array[n, char]
  let c = await input.readInto(dst.addr, n).wait()
  return dst.toString

proc read(input: AsyncIpcHandle, timeout = 1.seconds): Future[
    string] {.async.} =
  # NOTE: read operation expect data *to be present in the pipe already*
  # if data is sent after the read is initiated, it stalls.
  # Read operations should always time-out.
  var size: array[sizeof(int), byte]
  var c = 0
  c = await input.readInto(size.addr, sizeof(int)).wait()
  if c != sizeof(int):
    raise newException(OSError, "Failed to read header from ipc.")
  let ln = cast[int](size)
  var dst = newSeq[byte](ln)
  c = await input.readInto(dst[0].addr, ln).wait()
  if c != ln:
    raise newException(OSError, "Failed to read content from ipc.")
  # result.add dst.toOpenArray(0, c - 1).toString
  result.add dst.toString

proc read(input: AsyncIpcHandle, _: bool, buffer = bufferSize): Future[seq[
    byte]] {.async.} =
  var dst: array[bufferSize, byte]
  var c, n: int
  while true:
    c = await input.readInto(dst.addr, bufferSize).wait()
    n += c
    result.add dst.toOpenArray(0, n - 1)
    if c < bufferSize or result[^1].char == '\0':
      break

proc sendd(data = "ok") =
    let input = open(ipcNameIn, sideReader)
    let output = open(ipcNameIn, sideWriter)
    while true:
        waitFor write(output, data)
        sleep(1000)

proc recvv() =
    let input = open(ipcNameIn, sideReader)
    let output = open(ipcNameIn, sideWriter)
    while true:
        try:
            let dst = waitFor read(input).wait(1.seconds)
            echo dst
        except AsyncTimeoutError:
            continue

import cligen
dispatchMulti([sendd], [recvv])
