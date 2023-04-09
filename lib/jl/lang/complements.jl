@doc "An example of complement type that is _not_ nothing."
struct NotNothing{T}
    v::T
    function NotNothing(::Nothing)
        throw(ArgumentError("Can't instantiate a NotNothing object with `nothing`"))
    end
    NotNothing(v::T) where {T} = new{T}(v)
end

# non parametrics
@doc "An example of complement type that is _not_ nothing."
struct NotNothingg
    v
    NotNothingg(v) = begin
    isnothing(v) && throw(ArgumentError("Can't instantiate a NotNothing object with `nothing`"))
    new(v)
    end
end
