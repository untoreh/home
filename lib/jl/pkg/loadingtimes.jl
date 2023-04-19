# This code allows to list all invalidations that happen within a list of imports statements.
# By sorting import statements from most downstream to most upstream methods invalidations can be reduced
# and the load time of the package improved

using SnoopCompileCore
snoops = []
@time for line in (
    quote
     # INSERT IMPORTS STATEMENTS HERE
    end
).args
    line isa LineNumberNode && continue
    push!(snoops, (line, @snoopr eval(line)))
end
using SnoopCompile
invs = [(line, uinvalidated(s)) for (line, s) in snoops]
counts = [(i[1], length(i[2])) for i in invs]
display(counts)
println("sum:")
display(sum(c[2] for c in counts))
# display(sum(invs(s) for (_, s) in snoops))
# trees = invalidation_trees(invalidations)

