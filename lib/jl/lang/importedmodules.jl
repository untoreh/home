function find_imported_modules(m::Module, depth=(nameof(m),), done=Set{Module}())
    push!(done, m)
    # Get the names of the children modules of m
    children = names(m, all=true, imported=true)
    # Loop through the children names
    result = Tuple[]
    for x in children
        # Check if x is a valid module name
        isdefined(m, x) || continue
        child = getglobal(m, x)
        if child isa Module
            child ∈ done && continue
            name = nameof(child)
            push!(result, (depth..., name))
            # Find all modules imported by child using the modules_importing function
            imported = find_imported_modules(child, (depth..., name), done)
            append!(result, imported)
        end
    end
    # Return the result set
    return result
end
