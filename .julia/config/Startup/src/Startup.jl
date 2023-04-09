module Startup

loadpy() = include("$(ENV["HOME"])/.julia/config/python.jl")

function debug!(del=0)
    if in("JULIA_DEBUG", keys(ENV))
        delete!(ENV, "JULIA_DEBUG")
    else
        ENV["JULIA_DEBUG"] = "all"
    end
    deletehistory!(del)
end

@doc "Remove the last `n` commands from the julia repl history file."
function deletehistory!(n=1)
    @eval using REPL
    for _ in 1:n
        pop!(Base.active_repl.mistate.current_mode.hist.history)
    end
    open(REPL.find_hist_file(), "r+") do s
        init_pos = pos = position(seekend(s))
        while n > 0
            seek(s, pos - 8)
            c = read(s, 8)
            # counting  commands
            if c == b"# time: "
                seek(s, pos - 9)
                nl = read(s, 1)
                nl == b"\n" && (n -= 1)
            end
            pos -= 1
        end
        if pos != init_pos
            seekstart(s)
            truncate(s, pos - 8) # FIXME: 8 or 16?
            seekend(s)
            println(s, "")
        end
    end
    nothing
end

@doc "Print all keys of a container."
macro keys(c)
    quote
        println.(keys($(esc(c))))
        nothing
    end
end

const _globlock = Base.Semaphore(1)
macro show!(args...)
    doshow = :(@show)
    append!(doshow.args, args)
    quote
        Base.acquire(_globlock)
        try
            $(esc(doshow))
        finally
            Base.release(_globlock)
        end
    end
end

function display!(args...)
    Base.acquire(_globlock)
    try
        display(args...)
    finally
        Base.release(_globlock)
    end
end

function myreplinit(repl)
    try
        @eval Main begin
            using OhMyREPL
            @async begin
                # reinstall keybindings to work around https://github.com/KristofferC/OhMyREPL.jl/issues/166
                sleep(1)
                OhMyREPL.Prompt.insert_keybindings()
                # don't use autocomplete, as it clashes with tab-completion
                OhMyREPL.enable_autocomplete_brackets(false)
            end
            @eval Main.colorscheme!("OneDark")
        end
    catch e
        @warn "error while importing OhMyREPL" e
    end
    if isempty(get(ENV, "JULIA_PRECOMP_OVERRIDE", ""))
        ENV["JULIA_NOPRECOMP"] = (:PingPong,
            :Scrapers,
            :Engine,
            :Backtest,
            :Strategies,
            :Instances,
            :Collections,
            :Executors,
            :Live,
            :Paper,
            :Watchers,
            :Plotting,
            :Stats)
    end
    debug!()
    debug!()
end

__init__() = begin
    atreplinit(myreplinit)
end

using SnoopPrecompile

@precompile_setup begin
    using Suppressor
    using Term.Progress
    const comp_task = Ref{Task}()
    const init_error = Ref{Any}()
    @precompile_all_calls begin
        @eval using Pkg: Pkg as Pkg
        @eval using Suppressor
        @eval begin
            @suppress begin
                println("Ignore following...")
                @show!(1)
                display!("1")
            end
        end
        __init__()
        @eval begin
            using REPL
            using OhMyREPL
            OhMyREPL.__init__()
            using JLLWrappers
            using OhMyREPL.JLFzf
            using OhMyREPL.JLFzf.fzf_jll

        end
        home = ENV["HOME"]
        cd(dirname(dirname(pathof(Startup))))
        Pkg.activate(".")
        include(joinpath(home, ".doom.d", "langs", "revise.jl"))
        revise!(false)
        include("precompile.jl")
    end
end

end
