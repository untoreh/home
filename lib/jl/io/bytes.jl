buffers = UInt8[]

to_bytes(s::AbstractString)::Vector{UInt8} = begin
    buf = try
        pop!(buffers)
    catch error
        if error isa ArgumentError
            IOBuffer()
        else
            rethrow(error)
        end
    end
    try
        write(buf, s)
        seekstart(buf)
        take!(buf)
    catch
        truncate(buf, 0)
    finally
        push!(buffers, buf)
    end
end
