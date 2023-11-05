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
    @eval using Term.Progress: ProgressBar, addjob!, stop!, update!, start!, render
    Core.eval(Main, :(Startup._dorevise($dotask)))
end

function _dorevise(dotask=true)
    # # Don't precompile packages when using revise.
    ENV["JULIA_PKG_PRECOMPILE_AUTO"] = false
    isnothing(get(ENV, "JULIA_SKIPINIT", nothing)) || return
    proj = Pkg.project()
    invokelatest(prompt!)
    pbar = ProgressBar(columns=:default, transient=true)
    prog = addjob!(pbar, description="Compiling...", N=8)
    prog!(i=1) = begin
        update!(prog; i)
        render(pbar)
    end
    start!(pbar)
    if !isnothing(proj.name)
        @sync begin
            @async Core.eval(Main, :(begin
                using Revise
                if !isnothing(get(ENV, "REVISE_LOGGING", nothing))
                    rlogger = Revise.debug_logger()
                end
            end))
            prog!() # 2
        end
        prog!() # 3
        mod = Symbol(proj.name)
        if dotask
            comp_task[] = @async begin
                try
                    prog!() # 4
                    Pkg.precompile(io=Base.devnull)
                    prog!() # 5
                    Core.eval(Main, :(using $mod))
                    prog!() # 6
                    Core.eval(Main, :(using Base.Meta))
                    prog!() # 7
                catch e
                    init_error[] = () -> (showerror(stdout, e); e)
                    @error "Failed to load $(mod)!"
                    flush(stdout)
                    # OhMyREPL.input_prompt!(project_prompt("error!"))
                finally
                    stop!(pbar)
                end
                prog!() # 8
            end
        else
            showerror(stdout, ErrorException("ops"))
            prog!(5)
            stop!(pbar)
        end
    else
        stop!(pbar)
        @warn "No project found, not loading Revise."
    end
end

mbrevise(mod) = (isdefined(Main, Symbol(mod)) && isa(mod, Module)) ? revise(mod) : revise()

function recursive_revise(path)
    for (mod, file) in Revise.included_files
        if occursin(path, file)
            Revise.track(mod, file)
        end
    end
end

const REVISE_TASK = Ref{Task}()

function recursive_revise(mod::Module)
    @eval using Suppressor: @suppress
    recursive_revise(dirname(pathof(mod)))
    if isassigned(REVISE_TASK) && !istaskdone(REVISE_TASK[])
        Base.throwto(REVISE_TASK[], InterruptException())
    end
    @eval REVISE_TASK[] = @async Revise.entr(String[], [$mod]) do
        @suppress Revise.revise($mod)
    end
end

const rrevise = recursive_revise

export revise!, comp_task, init_error, mbrevise, rrevise
