using Pkg

# Don't precompile packages when using revise.
ENV["JULIA_PKG_PRECOMPILE_AUTO"] = false
include("dev_packages.jl")
using Suppressor
using Term.Progress
let proj = Pkg.project()
    pbar = ProgressBar(columns=:minimal, transient=true)
    prog = addjob!(pbar, description="Compiling...", N=9)
    prog!() = begin
        update!(prog)
        render(pbar)
    end
    # OhMyREPL.input_prompt!(project_prompt("compiling..."))
    start!(pbar)
    if !isnothing(proj.name)
        @sync begin
            @async @eval using Revise
            prog!() # 2
            @async @eval using OhMyREPL
        end
        prog!() # 3
        mod = Symbol(proj.name)
        global comp_task
        comp_task = @async begin
            try
                prog!() # 4
                @suppress begin
                    Pkg.precompile()
                end
                prog!() # 5
                @eval using $mod
                prog!() # 6
                @eval using Base.Meta
                prog!() # 7
                eval(Meta.parse("Revise.revise($mod)"))
                prog!() # 8
            catch e
                global last_error
                last_error = () -> showerror(stdout, e)
                OhMyREPL.input_prompt!(project_prompt("error!"))
            finally
                stop!(pbar)
            end
            prog!() # 9
        end
    else
        stop!(pbar)
        @warn "No project found, not loading Revise."
    end
end;
