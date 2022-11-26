import std/sequtils

type
  Generator[T] = object
    current: int
    s: seq[T]

proc newGen[T](s: sink openarray[T]): Generator[T] =
  result.s = s.toSeq
  result.current = result.s.low

proc next[T](g: var Generator[T]): lent T =
  result = g.s[g.current]
  g.current.inc
  if unlikely(g.current > g.s.high):
    g.current = g.s.low

let wow = @[0 , 1, 2 ]

var g = newGen(wow)
echo next(g)
echo next(g)
echo next(g)
echo next(g)
