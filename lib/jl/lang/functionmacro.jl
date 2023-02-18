macro wfunc(ex)
    @assert ex.head ∈ (:(=), :function)
    @assert ex.args[1].head == :call
    fdef = ex.args[1]
    fname = fdef.args[1]
    @assert fname isa Symbol "Don't prefix function name ($fname) with a module."
    fdef.args[1] = Expr(:(.), Symbol(@__MODULE__), QuoteNode(fname))
    ex = @macroexpand1 ex
    quote
        @eval $(ex)
    end
end
