
macro defun(fname)
    fname = esc(Symbol(fname))
    ex1 = quote
        function func(ai::AssetInstance, date; kwargs...)
            func(ai.ohlcv, date; kwargs...)
        end
    end
    ex1.args[2].args[1].args[1] = fname
    ex1.args[2].args[2].args[3].args[1] = fname
    ex2 = quote
        function func(s::Strategy, ai::AssetInstance, date; tf=s.timeframe, kwargs...)
            func(ai.data[tf], date; kwargs...)
        end
    end
    ex2.args[2].args[1].args[1] = fname
    ex2.args[2].args[2].args[3].args[1] = fname
    quote
        $ex1
        $ex2
    end
end
