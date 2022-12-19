let pkgs = ["JuliaInterpreter", "ProtoStructs"]
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
