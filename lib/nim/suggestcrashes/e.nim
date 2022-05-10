import macros,
       locks

template lockedStore*(name: untyped): untyped {.dirty.} =
    type
        `Lock name Obj`[K, V] = object
            lock: Lock
            storage {.guard: lock.}: name[K, V]
        `Lock name`*[K, V] = ptr `Lock name Obj`[K, V]

    proc `lock name Impl`*[K, V](store: name[K, V]): `Lock name`[K, V] =
        result = createShared(`Lock name Obj`[K, V])
        initLock(result.lock)
        withLock(result.lock):
            result.storage = store

    template `init Lock name`*[K, V](args: varargs[untyped]): `Lock name`[K, V] =
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

    proc `[]=`*[K, V](tbl: `Lock name`[K, V], k: K, v: V) =
        withLock(tbl.lock):
            tbl.storage[k] = v

when isMainModule:
    import tables
    lockedStore(Table)
    let x = initLockTable[string, string](100)
    x["a"] = "123" # OFFENDING LINE
