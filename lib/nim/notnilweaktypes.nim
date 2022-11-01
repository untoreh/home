import std/times, parseutils, strutils

type
  A = ref int
  ANotNil = distinct A

converter toNotNil(a: A): ANotNil =
  cast[ANotNil](
    if a.isnil:
      new(A)
    else:
      a
  )

proc doa(a: ANotNil) =
  echo a.repr

var v: A
# var v = new(A)
# v[] = 1
doa(v)
