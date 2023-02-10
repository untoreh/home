@doc """Defines a mutable struct where fields are `const`ed by default. Types that end with `&mut` are considered mutable.
```
@mutable struct Point
    x::T&mut
    y::T
    z::T
end
mutable struct Point
      x::T
      const y::T
      const z::T
end
```
"""
macro mutable(ex)
    @assert ex.head == :struct
    ex.args[1] = true # set mutable struct bool flag to true
    types = ex.args[3].args
    for i in eachindex(types)
        child = types[i]
        child isa LineNumberNode && continue
        if child.head == :(::)
            types[i] = Expr(:const, child)
        elseif length(child.args) < 2 && continue
        elseif child.head == :call
            @assert child.args[1] == :& && child.args[3] == :mut
            types[i] = child.args[2]
        end
    end
    ex
end
