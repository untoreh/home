function functionname(ex)
    if ex isa Expr
        if ex.head == :function
            # Long form function definition
            if ex.args[1] isa Symbol
                return ex.args[1]
            elseif ex.args[1] isa Expr && ex.args[1].head == :call
                return ex.args[1].args[1]
            end
        elseif ex.head == :(=) && ex.args[1] isa Expr && ex.args[1].head == :call
            # Short form function definition
            return ex.args[1].args[1]
        elseif ex.head == :->
            # Closure syntax
            if ex.args[1] isa Symbol
                return ex.args[1]
            elseif ex.args[1] isa Expr && ex.args[1].head == :tuple
                return :closure # no function name for closures with multiple arguments
            end
        end
    end
    return nothing
end
