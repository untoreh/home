import std/[importutils, tables]

privateAccess(OrderedTable)

type
  OrderedTableIterator[K, V] = object
    tbl: OrderedTableRef[K, V]
    next: int
  OrderedTableIteratorRef[K , V] = ref OrderedTableIterator[K, V]

privateAccess(OrderedTable)
proc init*[K, V](_: typedesc[OrderedTableIterator]): OrderedTableIterator[K, V] =
  new(result.tbl)
  result.tbl[] = initOrderedTable[K, V]()
  result.next = result.tbl.first

proc init*[K, V](_: typedesc[OrderedTableIterator], tbl: OrderedTableRef[K, V]): OrderedTableIterator[K, V] =
  assert not tbl.isnil
  result.tbl = tbl
  result.next = result.tbl.first

proc next*[K, V](t: var OrderedTableIterator[K, V]): V =
  let this =
    if t.next == -1: t.tbl.data[t.tbl.first]
    else: t.tbl.data[t.next]
  result = this.val
  t.next = this.next
