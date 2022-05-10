import
    tables,
    locks

export tables,
       locks

proc put*[T, K, V](t: T, k: K, v: V): V = (t[k] = v; v)

type PathLock* = Table[string, Lock]
var locksBuffer*: seq[ref Lock]

proc get*(b: var seq[ref Lock]): ref Lock =
    try:
        return b.pop()
    except:
        discard

proc acquireOrWait*(pl: PathLock, k: string): bool =
    try:
        withLock(pl[k][]):
            discard
        result = false
    except KeyError:
        pl.put(k, locksBuffer.get)[].acquire() # OFFENDING LINE
        result = true
