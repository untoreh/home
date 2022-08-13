
template print(s: varargs[string, `$`]) =
    when defined(debugNimpy):
        for x in s:
            stdout.write x
        stdout.flushFile
