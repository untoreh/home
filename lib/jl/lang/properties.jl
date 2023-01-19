# This is not really idiomatic code
struct B
    c
end
struct A
    b::B
end

function getproperty(a::A, s::Symbol)
    s ∈ fieldnames(B) && return getproperty(getfield(a, :b), s)
    getfield(a, s)
end
