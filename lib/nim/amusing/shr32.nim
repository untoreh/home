import strformat, strutils, parseutils
proc shr32(x, bits: uint32): uint32 =
  ## string based shr32
  if bits <= 0: return x
  if bits >= 32: return 0
  var bin = fmt"{uint64(x):b}" # get binary representation
  let ln = bin.len
  if ln > 32:
    bin = bin[ln-32..<32]
  elif ln < 32:
    bin = fmt"{bin:0>32}" # pad with zeroes to 32 digits
  try:
    let shifted = bin[0..<32-bits]
    var v: uint32
    discard parseBin(fmt"{shifted:0>32}", v)
    return v
  except:
    return 0'u32

import timeit
timeGo(10, 10):
  discard shr32(123'u32, 3'u32)

timeGo(10, 10):
  discard `shr`(123'u32, 3'u32)

echo "test.nim:59"
# echo charCodeAt(v, 2)
