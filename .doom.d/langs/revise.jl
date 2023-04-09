using Pkg

revise_logs() = @eval begin
    using Base.CoreLogging: Debug
    rlogs = filter(r -> r.level == Debug, rlogger.logs)
    open("/tmp/revise.logs", "w") do io
        for log in rlogs
            println(io, log)
        end
    end
end

function revise!(dotask=true)
    # # Don't precompile packages when using revise.
    ENV["JULIA_PKG_PRECOMPILE_AUTO"] = false
    include(joinpath(@__DIR__, "dev_packages.jl"))
    isnothing(get(ENV, "JULIA_SKIPINIT", nothing)) || return
    proj = Pkg.project()
    invokelatest(prompt!)
    pbar = ProgressBar(columns=:default, transient=true)
    prog = addjob!(pbar, description="Compiling...", N=9)
    prog!(i=1) = begin
        update!(prog; i)
        render(pbar)
    end
    start!(pbar)
    if !isnothing(proj.name)
        @sync begin
            @async @eval Main begin
                using Revise
                if !isnothing(get(ENV, "REVISE_LOGGING", nothing))
                    rlogger = Revise.debug_logger()
                end
            end
            prog!() # 2
            @async @eval Main using OhMyREPL
        end
        prog!() # 3
        mod = Symbol(proj.name)
        if dotask
            comp_task[] = @async begin
                try
                    prog!() # 4
                    @suppress begin
                        Pkg.precompile()
                    end
                    prog!() # 5
                    @eval Main using $mod
                    prog!() # 6
                    @eval Main using Base.Meta
                    prog!() # 7
                    eval(Meta.parse("Revise.track($mod)"))
                    prog!() # 8
                    includestartup()
                catch e
                    init_error[] = () -> (showerror(stdout, e); e)
                    OhMyREPL.input_prompt!(project_prompt("error!"))
                finally
                    stop!(pbar)
                end
                prog!() # 9
            end
        else
            showerror(stdout, ErrorException("ops"))
            invokelatest(includestartup)
            prog!(6)
            stop!(pbar)
        end
    else
        stop!(pbar)
        @warn "No project found, not loading Revise."
    end
end
