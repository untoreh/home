elements(coll) = coll
elements(coll::NamedTuple) = fieldnames(coll)
elements(coll::Dict) = keys(coll)

macro haselement(tuple, x)
    nt = @eval __module__ $tuple
    ex = quote
        this = $(esc(x))
    end
    exf = Expr(:||, :(this == $(QuoteNode(elements(nt)[1]))))
    push!(ex.args, exf)
    for f in elements(nt)[2:end]
        e = Expr(:||, :(this == $(QuoteNode(f))))
        push!(exf.args, e)
        exf = e
    end
    ex
end
