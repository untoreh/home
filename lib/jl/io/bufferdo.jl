IOBuffer(f::Function, args...;kwargs...) = begin
    buf = IOBuffer(args...; kwargs...)
    try
        f(buf)
    finally
        close(buf)
    end
end
