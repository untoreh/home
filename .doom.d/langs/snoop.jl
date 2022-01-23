using Pkg
let proj = Pkg.project()
    projpath = dirname(proj.path)
    testpath = joinpath(projpath, "test")
    if "SnoopCompile" ∈ keys(proj.dependencies)
        @eval using SnoopCompile; using ProfileView
    else
        @eval using TOML
        testcfg = joinpath(testpath, "Project.toml")
        if !ispath(testcfg) || "SnoopCompile" ∉ keys(TOML.parsefile(testcfg)["deps"])
            mkpath(testpath)
            Pkg.activate(testpath)
            Pkg.add("SnoopCompile")
            Pkg.add("ProfileView")
            Pkg.activate(projpath)
        end
        push!(LOAD_PATH, testpath)
        @eval using SnoopCompile; using ProfileView
    end
    arg_proj = "--project=$projpath"
    scriptpath = joinpath(projpath, "deps", "SnoopCompile", "script.jl")
    # eval(Meta.parse("using $(proj.name)"))
    @eval function pkgsnoop()
        @info "Snooping..."
        logfile = "snoop.log"
        precdir = "deps/precompiles"
        rm(precdir; force=true, recursive=true)
        SnoopCompile.@snoopc [$arg_proj] logfile begin
            ENV["SNOOP_COMPILER"] = "1"
            include($scriptpath)
            # eval(Meta.parse("using $($proj.name)"))
        end
        data = SnoopCompile.read(logfile)
        pc = SnoopCompile.parcel(reverse!(data[2]))
        SnoopCompile.write(precdir, pc)
        @info "Snooped!"
    end
end
