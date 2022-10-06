import std/[sharedtables, locks, sets]

proc `[]`*[K, V](t: var SharedTable[K, V], k: K): V = t.mget(k)
proc `get`*[K, V](t: var SharedTable[K, V], k: K): V = t.mget(k)
proc `put`*[K, V](t: var SharedTable[K, V], k: K, v: V): V =
    t[k] = v
    return v

type SharedHashSet*[T] = ref object
    data: HashSet[T]
    lock: Lock

proc init*[T](s: SharedHashSet[T]) =
    s.data = initHashSet[T]()
    initLock(s.lock)

proc contains*[T](d: SharedHashSet[T], v: T): bool =
    withLock(d.lock):
        result = v in d.data
proc incl*[T](d: SharedHashSet[T], v: T) =
    withLock(d.lock):
        d.data.incl(v)
proc excl*[T](d: SharedHashSet[T], v: T) =
    withLock(d.lock):
        d.data.excl(v)

# PathLocker, like sharedtables, one lock per key
type PathLock* = LockTable[string, ref Lock]
var locksBuffer* {.threadvar.}: seq[ref Lock]

proc initPathLock*(): PathLock =
    initLockTable[string, ref Lock]()

proc addLocks*() =
    for _ in 0..<100:
        locksBuffer.add new(Lock)

proc get*(b: var seq[ref Lock]): ref Lock =
    try:
        return b.pop()
    except:
        addLocks()
        return b.pop()

proc contains*(pl: PathLock, k: string): bool = k in pl

proc acquireOrWait*(pl: PathLock, k: string): bool =
    try:
        # waited
        withLock(pl[k][]):
            discard
        result = false
    except KeyError:
        # acquired
        pl.put(k, locksBuffer.get)[].acquire()
        result = true

proc release*(pl: PathLock, k: string) =
    try:
        pl[k][].release()
    except KeyError: discard
