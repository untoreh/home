using Pkg

# Don't precompile packages when using revise.
ENV["JULIA_PKG_PRECOMPILE_AUTO"] = false
include("dev_packages.jl")
using Suppressor
let proj = Pkg.project()
    if !isnothing(proj.name)
        @sync begin
            @async @eval using Revise
            @async @eval using OhMyREPL
        end
        mod = Symbol(proj.name)
        @async begin
            OhMyREPL.input_prompt!(project_prompt("compiling..."))
            try
                @suppress begin
                    Pkg.precompile()
                end
                @eval using $mod
                @eval using Base.Meta
                eval(Meta.parse("Revise.revise($mod)"))
                OhMyREPL.input_prompt!(project_prompt())
            catch
                OhMyREPL.input_prompt!(project_prompt("error!"))
            end
        end
    else
        @warn "No project found, not loading Revise."
    end
end;
