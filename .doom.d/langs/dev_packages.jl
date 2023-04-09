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
        let out = @capture_out begin
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

prompt!(v="") = OhMyREPL.input_prompt!(project_prompt(v))
macro activate(place::String="")
    quote
        Pkg.activate($place)
        OhMyREPL.input_prompt!(project_prompt())
    end
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
