
function fromstruct(c::T) where T
    names = fieldnames(T)
    nt = NamedTuple{names, Tuple{fieldtypes(T)...}}
    t = (getfield(c, f) for f in names)
    nt(t)
end
