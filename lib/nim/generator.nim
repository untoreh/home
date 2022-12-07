import std/sequtils

type
  Generator[T] = object
    current: int
    s: seq[T]

proc newGen[T](s: sink openarray[T]): Generator[T] =
  result.s = s.toSeq
  result.current = result.s.low

proc next*[T](g: var Generator[T]): lent T =
  result = g.s[g.current]
  if unlikely(g.current >= g.s.high):
    g.current = g.s.low
  else:
    g.current.inc

let wow = @[0 , 1, 2 ]

var g = newGen(wow)
echo next(g)
echo next(g)
echo next(g)
echo next(g)

## with mutation
type
  Mutator[T, V] = proc(el: T): V {.gcsafe.}
  Generator*[T, V] = object
    current: int
    s: seq[T]
    get: Mutator[T, V]


proc newGen*[T, V](s: sink openarray[T], get: Mutator[T, V]): Generator[T, V] =
  result.s = s.toSeq
  result.get = get
  result.current = result.s.low

proc default*[T, V](_: typedesc[Generator[T, V]]): Generator[T, V] =
  result.current = -1
  echo result.s.high

proc next*[T, V](g: var Generator[T, V]): V =
  if likely(g.s.high != -1):
    result = g.get(g.s[g.current])
    if unlikely(g.current >= g.s.high):
      g.current = g.s.low
    else:
      g.current.inc
