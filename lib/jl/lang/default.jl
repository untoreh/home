istypeorval(t::Type, v) = v isa t
istypeorval(t::Type, v::Type) = v <: t

@doc "Get a default value for common types."
default(t::Type) = begin
    if applicable(zero, (t,))
        zero(t)
    elseif applicable(empty, Tuple{t})
        empty(t)
    elseif istypeorval(AbstractString, t)
        ""
    elseif istypeorval(AbstractChar, t)
        '\0'
    elseif istypeorval(Tuple, t)
        ((default(ft) for ft in fieldtypes(t))...,)
    elseif t isa Function
        (_...) -> nothing
    elseif applicable(t)
        t()
    else
        throw(ArgumentError("No default value for type: $t"))
    end
end
