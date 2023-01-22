# Inline all functions defined in a block
function is_anon_func(ex)
    hasproperty(ex, :args) &&
        length(ex.args) > 1 &&
        Base.is_function_def(ex.args[2])
end

macro inlineall(block)
    for (n, ex) in enumerate(block.args)
        if Base.is_function_def(ex)
            block.args[n] = :(@inline $ex)
        elseif is_anon_func(ex)
            block.args[n].args[2] = :(@inline $(ex.args[2]))
        end
    end
    block
end
