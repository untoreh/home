module ReusableObjects

const objects = Dict{Symbol,Any}()
@doc """ Returns a possible already allocated object of type `t`.

```julia
o = object!(Dict{String, String}, :abc)
@assert o isa Dict{String, String}
"""
function object!(t::Type, k::Symbol, default=t)
    obj_vec = @something get(objects, k, nothing) begin
        v = t[default()]
        objects[k] = v
        v
    end
    isempty(obj_vec) && return default()
    pop!(obj_vec)
end
@doc "Empties allocated objects for `k`."
clearkey!(k::Symbol) = k ∈ objects && empty!(objects[k])
_doclear!(v) = applicable(empty!, v) && empty!(v)
@doc """ Bind `sym` instantiate a type `t` and evaluate an expression.

Example:
```julia
@with myvar Dict{String, String} :abc begin
    print(typeof(myvar))
    @assert isempty(myvar)
end
```
"""
macro with(sym, t, k, args...)
    t = esc(t)
    k = esc(k)
    @assert !isempty(args) "No expression given."
    if length(args) == 1
        expr = args[1]
        default = t
    else
        default = esc(args[1])
        expr = args[2:end]
    end
    sym = esc(sym)
    quote
        $sym = $(object!)($t, $k, $default)
        $(esc(expr))
        $(@__MODULE__).@discard! $k $sym
    end
end
macro discard!(k, v)
    v = esc(v)
    k = esc(k)
    quote
        $(_doclear!)($v)
        push!($(objects)[$k], $v)
        $v = nothing
    end
end

# function testloop(t::Type, k::Symbol, v=t)
#     for _ in 1:100000
#         o = object!(t, k, v)
#         @discard! k o
#     end
# end

# function testloop3(t::Type, k::Symbol, v=t)
#     for _ in 1:100000
#         @with o t k begin end
#     end
# end

# function testloop2(t::Type, k, v=t)
#     for _ in 1:100000
#         o::Union{Nothing,t} = v()
#         o = nothing
#     end
# end
end
