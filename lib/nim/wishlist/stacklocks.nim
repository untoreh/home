import std/macros

let l1 = 0
var g {.guard: l1.} = false

# macro guarded[T](l: T, code: untyped): untyped =
#   result = quote do:
#     # var g {.global, guard: l1} = false
#     g = true
#     `code`

proc a(): int =
  g = true
  1 + 1

# this should compile, but it doesn't because nim doesn't
# recurse the stackframe to check
# previous locks acquisitions
proc aa(): int =
  {.locks: [l1].}:
    return a()

let r = aa()
echo r
