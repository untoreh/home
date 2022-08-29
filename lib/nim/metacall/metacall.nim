
# {.push cdecl, dynlib: "/gnu/store/51cqrsi1w1ih6y411a7b6dgn692zyp82-metacall-0.5.20/lib/libmetacall.so",  importc.}
# {.push cdecl, dynlib: "/gnu/store/51cqrsi1w1ih6y411a7b6dgn692zyp82-metacall-0.5.20/lib/libmetacall.so",  importc.}
{.push header: "metacall/metacall.h", header: "metacall/metacall_value.h".}
var
  METACALL_BOOL*: cint
  METACALL_CHAR*: cint
  METACALL_SHORT*: cint
  METACALL_INT*: cint
  METACALL_LONG*: cint
  METACALL_FLOAT*: cint
  METACALL_DOUBLE*: cint
  METACALL_STRING*: cint
  METACALL_BUFFER*: cint
  METACALL_ARRAY*: cint
  METACALL_MAP*: cint
  METACALL_PTR*: cint
  METACALL_FUTURE*: cint
  METACALL_FUNCTION*: cint
  METACALL_NULL*: cint
  METACALL_CLASS*: cint
  METACALL_OBJECT*: cint

{.pop.}
{.push cdecl, header: "metacall/metacall.h", importc.}

proc metacall_value_create_int*(i: cint): pointer
proc metacall_value_create_double*(d: cdouble): pointer
# # TODO: Add more types

proc metacall_value_to_int*(v: pointer): cint
proc metacall_value_to_double*(v: pointer): cdouble
# # TODO: Add more types

proc metacall_value_destroy*(v: pointer): void

proc metacall_initialize*(): cint
proc metacall_load_from_file*(tag: cstring, paths: cstringArray, size: csize_t,
    handle: pointer): cint
proc metacall_load_from_memory*(tag: cstring, buffer: cstring, size: csize_t,
    handle: pointer): cint
proc metacallv_s*(name: cstring, args: pointer, size: csize_t): pointer
proc metacall_destroy*(): cint

{.pop.}


# {.passL: "-lmetacall".}

# {.pragma: metacallHeader, header: "metacall/metacall.h".}
# {.pragma: metacallValueHeader, header: "metacall/metacall_value.h".}
# {.pragma: metacallProc, metacallHeader, metacallValueHeader, importc: "metacall_$1".}
# {.pragma: metacallCallProc, metacallHeader, importc: "metacall$1".}
# {.pragma: metacallConst, metacallHeader, metacallValueHeader, importc: "METACALL_$1".}

# var
#   BOOL* {.metacallConst.}: cint
#   CHAR* {.metacallConst.}: cint
#   SHORT* {.metacallConst.}: cint
#   INT* {.metacallConst.}: cint
#   LONG* {.metacallConst.}: cint
#   FLOAT* {.metacallConst.}: cint
#   DOUBLE* {.metacallConst.}: cint
#   STRING* {.metacallConst.}: cint
#   BUFFER* {.metacallConst.}: cint
#   ARRAY* {.metacallConst.}: cint
#   MAP* {.metacallConst.}: cint
#   PTR* {.metacallConst.}: cint
#   FUTURE* {.metacallConst.}: cint
#   FUNCTION* {.metacallConst.}: cint
#   NULL* {.metacallConst.}: cint
#   CLASS* {.metacallConst.}: cint
#   OBJECT* {.metacallConst.}: cint


# proc value_create_int*(i: cint): pointer {.metacallProc.}
# proc value_create_double*(d: cdouble): pointer {.metacallProc.}
# # TODO: Add more types

# proc value_to_int*(v: pointer): cint {.metacallProc.}
# proc value_to_double*(v: pointer): cdouble {.metacallProc.}
# # TODO: Add more types

# proc value_destroy*(v: pointer): void {.metacallProc.}

# proc initialize*(): cint {.metacallProc.}
# proc load_from_file*(tag: cstring, paths: cstringArray, size: csize_t, handle: pointer): cint {.metacallProc.}
# proc load_from_memory*(tag: cstring, buffer: cstring, size: csize_t, handle: pointer): cint {.metacallProc.}
# proc v_s*(name: cstring, args: pointer, size: csize_t): pointer {.metacallCallProc.}
# proc destroy*(): cint {.metacallProc.}
