using Pkg
let proj = Pkg.project()
    isnothing(proj.name) ||
        if "Revise" ∈ keys(proj.dependencies)
            @eval using Revise
            Revise.revise(Symbol(proj.name))
        else
            @eval using TOML
            projpath = dirname(proj.path)
            testpath = joinpath(projpath, "test")
            testcfg = joinpath(testpath, "Project.toml")
            if !ispath(testcfg) || "Revise" ∉ keys(TOML.parsefile(testcfg)["deps"])
                mkpath(testpath)
                Pkg.activate(testpath)
                Pkg.add("Revise")
                Pkg.activate(projpath)
            end
            push!(LOAD_PATH, testpath)
            @eval using Revise
            Revise.revise()
            eval(Meta.parse("using $(proj.name)"))
        end
end
