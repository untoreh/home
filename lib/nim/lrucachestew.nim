import stew/[keyed_queue, results]


type
  LruCache[K, V] = object
    table: KeyedQueue[K, V]
    size: int
  LruCacheRef[K, V] = ref LruCache[K ,V]

proc newLruCache[K, V](maxSize: int): LruCacheRef[K, V] =
  new(result)
  init(result.table, maxSize)
  result.size = maxSize

proc `[]`[K, V](q: LruCacheRef[K, V], k: K): Result[V, void] = q.table.lruFetch(k)

proc `[]=`[K, V](q: LruCacheRef[K,V], k: K, v: V) = discard q.table.lruAppend(k, v, q.size)

let cache = newLruCache[string, int](3)
cache["Asd"] = 1
cache["pls"] = 1
cache["wow"] = 1
cache["eheh"] = 1


echo cache["pls"].isOk
