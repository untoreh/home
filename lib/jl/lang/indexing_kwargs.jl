function Base.getindex(pf::Portfolio;
                       b::Union{Symbol,Nothing}=nothing,
                       q::Union{Symbol,Nothing}=nothing)
    @view pf.data[iscurr.(pf.data.asset, b, q), :]
end

iscurr(a::Asset, b::Symbol, ::Nothing = nothing) = a.bc == b
iscurr(a::Asset, ::Nothing, q::Symbol) = a.qc == q

# We use `nothing` fillers to infer multiple dispatch. Note that julia (1.8) can't dispatch on kwargs,
# therefore only one method accounting for all the cases (with union types) has to be defined.
