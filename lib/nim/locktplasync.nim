import chronos
import chronos/asyncsync
import utils
export asyncsync

template withLock*(l: AsyncLock, code: untyped): untyped =
  {.locks: [l].}:
    try:
        await l.acquire()
        code
    finally:
        l.release()

template asyncLockedStore*(name: untyped): untyped {.dirty.} =
    type
        `AsyncLock name Obj`[K, V] = object
            lock: AsyncLock
            storage {.guard: lock.}: name[K, V]
        `AsyncLock name`*[K, V] = ptr `AsyncLock name Obj`[K, V]

    proc `lock name Impl`*[K, V](store: name[K, V]): `AsyncLock name`[K, V] =
        result = createShared(`AsyncLock name Obj`[K, V])
        result.lock = newAsyncLock()
        {.locks: [result.lock].}:
            waitFor result.lock.acquire
            result.storage = store
            result.lock.release

    template `init AsyncLock name`*[K, V](args: varargs[untyped]): `AsyncLock name`[K, V] =
        var store: name[K, V]
        store = when compiles(`init name`):
                    when varargsLen(args) > 0:
                        `init name`[K, V](args)
                    else:
                        `init name`[K, V]()
                elif compiles(`new name`):
                    when varargsLen(args) > 0:
                        `new name`[K, V](args)
                    else:
                        `new name`[K, V]()
                else:
                    `name`[K, V]()
        `lock name Impl`[K, V](store)

    iterator items*[K, V](tbl: `AsyncLock name`[K, V]): (K, V) =
        withLock(tbl.lock):
            for (k, v) in tbl.storage.pairs():
                yield (k, v)

    iterator keys*[K, V](tbl: `AsyncLock name`[K, V]): K =
        withLock(tbl.lock):
            for k in tbl.storage.keys():
                yield k

    iterator pairs*[K, V](tbl: `AsyncLock name`[K, V]): K =
      try:
        await tbl.lock.acquire
        for k in tbl.storage.keys():
          tbl.lock.release
          yield k
          await tbl.lock.acquire
      finally:
        tbl.lock.release

    proc `[]=`*[K, V](tbl: `AsyncLock name`[K, V], k: K, v: V) =
        withLock(tbl.lock):
            tbl.storage[k] = v

    proc `put`*[K, V](tbl: `AsyncLock name`[K, V], k: K, v: V): Future[void] {.async.} =
        withLock(tbl.lock):
            tbl.storage[k] = v

    proc `[]`*[K, V](tbl: `AsyncLock name`[K, V], k: K): Future[V] {.async.} =
        withLock(tbl.lock):
            result = tbl.storage[k]

    proc contains*[K, V](tbl: `AsyncLock name`[K, V], k: K): Future[bool] {.async.} =
        withLock(tbl.lock):
            result = k in tbl.storage

    proc clear*[K, V](tbl: `AsyncLock name`[K, V]): Future[void] {.async.} =
        withLock(tbl.lock):
            clear(tbl.storage)

    proc len*[K, V](tbl: `AsyncLock name`[K, V]): Future[int] {.async.} =
        withLock(tbl.lock):
            result = tbl.storage.len

    proc get*[K, V](tbl: `AsyncLock name`[K, V], k: K, def: V): Future[V] {.async.} =
        withLock(tbl.lock):
            result = tbl.storage.getOrDefault(k, def)

    proc get*[K, V](tbl: `AsyncLock name`[K, V], k: K): Future[V] {.async.} =
        withLock(tbl.lock):
            result = tbl.storage.get(k)

    proc del*[K](tbl: `AsyncLock name`, k: K): Future[void] {.async.} =
        withLock(tbl.lock):
          {.cast(gcsafe).}:
            tbl.storage.del(k)

    proc pop*[K, V](tbl: var `AsyncLock name`, k: K, v: var V): Future[bool] {.async.} =
        withLock(tbl.lock):
            result = tbl.storage.pop(k, v)
