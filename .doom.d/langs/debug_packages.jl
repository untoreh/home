let pkgs = ["JuliaInterpreter", "ProtoStructs", "BenchmarkTools", "Suppressor"]
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
