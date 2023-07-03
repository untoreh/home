# This should not be needed since julia 1.9, just add the pkgs to the "main" env in `~/.julia/environments/v1.x
function hack_load_path(pkgs=["Revise", "JuliaInterpreter", "BenchmarkTools", "MacroTools", "ProtoStructs",
    "Suppressor", "Documenter", "StaticCompiler"])
    let pkgs =
            begin
                for p in pkgs
                    path = joinpath(DEPOT_PATH[1], "packages", p)
                    recent = pipeline(`ls  -Art $(path)`,
                        `tail -1`) |> x -> read(x, String) |> chomp
                    recent_path = joinpath(path, recent)
                    if recent_path ∉ LOAD_PATH
                        push!(LOAD_PATH, recent_path)
                    end
                end
            end
    end
end
# using ProtoStructs
@doc "Count how many instructions a compiled function generates."
macro countinstr(args)
    quote
        @eval using Suppressor: @capture_out
        @eval let out = @capture_out begin
                @code_native $args
            end
            n = 0
            for l in split(out)
                isletter(l[end]) && (n += 1)
            end
            n
        end
    end
end


function project_prompt(status="")
    if !isempty(status)
        "($(Pkg.project().name)) ($status) julia> "
    else
        "($(Pkg.project().name)) julia> "
    end
end

if isdefined(Startup, :OhMyRepl)
    prompt!(v="") = OhMyREPL.input_prompt!(project_prompt(v))
    macro activate(place::String="")
        quote
            Pkg.activate($place)
            OhMyREPL.input_prompt!(project_prompt())
        end
    end
else
    const prompt! = Returns(nothing)
end

function deserialize(v::AbstractVector)
    buf = IOBuffer(v)
    try
        v = Serialization.deserialize(buf)
        take!(buf)
        v
    finally
        close(buf)
    end
end

function includestartup()
    path = joinpath(pwd(), ".startup.jl")
    isfile(path) && @eval Main include($path)
end

@doc "Builds a flat list of all modules imported by the input module, preserving the full module path."
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

@doc "Updates the current repl display size, first arg updates LINES, optional second arg COLUMNS."
function displaysize!(args...)
    let ctx = Base.active_repl.options.iocontext
        if length(args) < 1
            delete!(ctx, :displaysize)
        else
            cols = length(args) < 2 ? displaysize()[1] : args[2]
            ctx[:displaysize] = (args[1], cols)
        end
    end
end

export find_imported_modules, @countinstr, displaysize!
