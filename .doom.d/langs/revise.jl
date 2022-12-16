using Pkg

include("debug_packages.jl")
let proj = Pkg.project()
    isnothing(proj.name) ||
        if "Revise" ∈ keys(proj.dependencies)
            @eval using Revise
            mod = Symbol(proj.name)
            @eval using $mod
            @eval using Base.Meta
            eval(Meta.parse("Revise.revise($mod)"))
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
            testpath ∉ LOAD_PATH && push!(LOAD_PATH, testpath)
            @eval using Revise
            Revise.revise()
            @eval using $(Symbol(proj.name))
            eval(Meta.parse("Revise.revise($(proj.name))"))
        end
end
