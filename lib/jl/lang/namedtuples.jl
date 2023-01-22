
# filter keys out of namedtuple
drop(nt::NamedTuple, key::Symbol) = Base.structdiff(nt, NamedTuple{(key,)})
drop(nt::NamedTuple, keys::NTuple{N,Symbol}) where {N} = Base.structdiff(nt, NamedTuple{keys})
