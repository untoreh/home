@doc "Define `@fromdict` locally, to avoid precompilation side effects."
macro define_fromdict!(force=false)
    quote
        (isdefined($(__module__), Symbol("@fromdict")) && !$force) || @eval begin
            @doc "This macro tries to fill a _known_ `NamedTuple` from an _unknown_ `Dict`."
            macro fromdict(nt_type, key_type, dict_var)
                ttype = eval(Base.Meta.parse("$__module__.$nt_type"))
                ktype = eval(Base.Meta.parse("$__module__.$key_type"))
                @assert ttype <: NamedTuple "First arg must be a namedtuple type."
                @assert ktype isa Type "Second arg must be the type of the dict keys."
                @assert applicable(ktype, Symbol()) "Can't convert symbols to $ktype."
                params = Expr(:parameters)
                ex = Expr(:tuple, params)
                for (fi, ty) in zip(fieldnames(ttype), fieldtypes(ttype))
                    p = Expr(
                        :kw,
                        fi,
                        :(convert($(ty), $(esc(dict_var))[$(convert(ktype, fi))])),
                    )
                    push!(params.args, p)
                end
                ex
            end
        end
    end
end

dotest(m::T) where {T} = @fromdict CgSymDerivative String m
dotest2(m::T) where {T} = NamedTuple(f => m[string(f)] for f in fieldnames(CgSymDerivative))

@generated fromdict(tuple, key, di) = begin
    params = Expr(:parameters)
    ex = Expr(:tuple, params)
    ttype = first(tuple.parameters)
    ktype = isempty(key.parameters) ? key : first(key.parameters)
    for (fi, ty) in zip(fieldnames(ttype), fieldtypes(ttype))
        p = Expr(:kw, fi, :(convert($ty, (di[$(convert(ktype, fi))]))))
        push!(params.args, p)
    end
    ex
end

function _fromdict(
    ::Type{ttype}, ::Type{ktype}, di, kconvfunc=convert, convfunc=convert
) where {ttype,ktype}
    t = (
        convfunc(ty, di[kconvfunc(ktype, fi)]) for
        (fi, ty) in zip(fieldnames(ttype), fieldtypes(ttype))
    )
    ttype(t)
end

dotest3(m::T) where {T} = fromdict(CgSymDerivative, String, m)

# the generated function (`dotest3`) has less allocations, but runs 0.02-0.04x slower than the macro (`dotest`).
# The for loop function (`dotest2`) runs the fastest (0.25x less then `dotest`) but consumes 2.5x times more memory.
# However this memory increase might become less relevant for large tuples.
