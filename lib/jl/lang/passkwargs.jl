function passkwargs(args...)
    return [Expr(:kw, a.args[1], a.args[2]) for a in args]
end

macro passkwargs(args...)
    kwargs = [Expr(:kw, a.args[1], a.args[2]) for a in args]
    return esc( :( $(kwargs...) ) )
end

macro example(name, args...)
    kwargs = passkwargs(args...)
    quote
        somefunc($name; $(kwargs...)
    end
end
