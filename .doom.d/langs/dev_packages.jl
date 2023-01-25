let pkgs = ["Revise", "JuliaInterpreter", "ProtoStructs", "BenchmarkTools",
            "Suppressor", "MacroTools", "Documenter", "StaticCompiler"]
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
# using ProtoStructs
@doc "Count how many instructions a compiled function generates."
macro countinstr(args)
    quote
        let out = @capture_out begin
            @code_native $args
        end
            n = 0
            for l in split(out)
                isletter(l[end]) && (n+=1)
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
macro activate(place::String="")
    quote
        Pkg.activate($place)
        OhMyREPL.input_prompt!(project_prompt())
    end
end
