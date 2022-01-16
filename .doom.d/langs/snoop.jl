using Pkg
let proj = Pkg.project()
    if "SnoopCompile" ∈ keys(proj.dependencies)
        @eval using SnoopCompile
    else
        @eval using TOML
        projpath = dirname(proj.path)
        testpath = joinpath(projpath, "test")
        testcfg = joinpath(testpath, "Project.toml")
        if !ispath(testcfg) || "SnoopCompile" ∉ keys(TOML.parsefile(testcfg)["deps"])
            mkpath(testpath)
            Pkg.activate(testpath)
            Pkg.add("SnoopCompile")
            Pkg.activate(projpath)
        end
        push!(LOAD_PATH, testpath)
        @eval using SnoopCompile
    end
    eval(Meta.parse("using $(proj.name)"))
end
