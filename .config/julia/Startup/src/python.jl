macro code_py(expr)
    quote
        if !isdefined(Main, :py_inspect)
            setglobal!(Main, :py_inspect, pyimport("inspect"))
        end
        print(Main.py_inspect.getsource($expr))
    end
end
