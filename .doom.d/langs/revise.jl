using Pkg

# Don't precompile packages when using revise.
ENV["JULIA_PKG_PRECOMPILE_AUTO"] = false
include("debug_packages.jl")
let proj = Pkg.project()
    isnothing(proj.name) ||
        if "Revise" ∈ keys(proj.dependencies)
            @async begin @eval using Revise
                mod = Symbol(proj.name)
                @eval using $mod
                @eval using Base.Meta
                doeval() = eval(Meta.parse("Revise.revise($mod)"))
                try
                    doeval()
                catch
                    Pkg.precompile()
                    doeval()
                end
            end
        else
            @async begin
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
end
