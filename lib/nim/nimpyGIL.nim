import nimpy {.all.}, dynlib, nimpy/py_lib
type PyGILState_STATE = enum PyGILState_LOCKED, PyGILState_UNLOCKED
type PyThreadState = ptr object
        interp: pointer

{.pragma: pyfunc, cdecl, gcsafe.}

initPyLibIfNeeded()
let
    m = pythonLibHandleForThisProcess()
    PyGILState_Ensure = cast[proc(): PyGILState_STATE {.pyfunc.}](m.symAddr("PyGILState_Ensure"))
    PyGILState_Release = cast[proc(s: PyGILState_STATE) {.pyfunc.}](m.symAddr("PyGILState_Release"))
    PyGILState_Check* = cast[proc(): bool {.pyfunc.}](m.symAddr("PyGILState_Check"))
    PyGILState_GetThisThreadState* = cast[proc(): PyThreadState {.pyfunc.}](m.symAddr("PyGILState_GetThisThreadState"))
    PyEval_SaveThread* = cast[proc(): PyThreadState {.pyfunc.}](m.symAddr("PyEval_SaveThread"))
    PyThreadState_Get* = cast[proc(): PyThreadState {.pyfunc.}](m.symAddr("PyThreadState_Get"))
    PyThreadState_Swap* = cast[proc(s: PyThreadState): PyThreadState {.pyfunc.}](m.symAddr("PyThreadState_Swap"))
    PyEval_RestoreThread* = cast[proc(s: PyThreadState) {.pyfunc.}](m.symAddr("PyEval_RestoreThread"))
    PyEval_AcquireThread* = cast[proc(s: PyThreadState) {.pyfunc.}](m.symAddr("PyEval_AcquireThread"))
    PyEval_ReleaseThread* = cast[proc(s: PyThreadState) {.pyfunc.}](m.symAddr("PyEval_ReleaseThread"))

let pyMainThread* = PyThreadState_Get()

template withGIL*(code): auto =
    var state: PyGILState_STATE
    try:
        state = Py_GILState_Ensure()
        code
    except: discard
    finally:
        try:
            Py_GILState_Release(state)
        except: discard

type GilLock* = object
        m: LibHandle
        s: PyGILState_STATE

proc initGilLock*(): ptr GilLock =
    result = create(GilLock)
    result.m = pythonLibHandleForThisProcess()

proc acquire*(g: ptr GilLock) = g.s = Py_GILState_Ensure()
proc release*(g: ptr GilLock) = Py_GILState_Release(g.s)

let pyGil* = initGilLock()
