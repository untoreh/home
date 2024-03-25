module Startup

const omr_enabled = Ref(false)
const PREV_DEBUG_PKGS = Ref("")

loadpy() = include("$(ENV["HOME"])/.julia/config/python.jl")

function debug!(del=0,)
    if haskey(ENV, "JULIA_DEBUG")
        PREV_DEBUG_PKGS[] = ENV["JULIA_DEBUG"]
        delete!(ENV, "JULIA_DEBUG")
    else
        ENV["JULIA_DEBUG"] = PREV_DEBUG_PKGS[]
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

_ohmyrepl() =
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

function myreplinit(repl)
    omr_enabled[] && _ohmyrepl()
    # double to toggle
    debug!()
    debug!()
end

compilepkg(sym::Symbol, compile_file="compile.jl", precompiles_dir="precomp" ) = begin
    @eval Main using CompileBot
    @eval Main begin
        name = string($(QuoteNode(sym)))
        this_path = Base.active_project() |> dirname |> dirname
        pkg_path = joinpath(this_path, name)
        botconfig = BotConfig(
            name;                            # package name (the one this configuration lives in)
            precompiles_rootpath=joinpath(pkg_path, $precompiles_dir),
            os=["linux"],
            exclusions = [],        # exclude functions (by name) that would be problematic if precompiled
            tmin=0.0
        )
        snoop_bot(
            botconfig,
            joinpath(pkg_path, $compile_file),
        )
    end
end

__init__() = begin
    atreplinit(myreplinit)
    atreplinit(customize_keys)
    if isdefined(Startup, :Revise)
        push!(Revise.dont_watch_pkgs, :Startup)
        Revise.silence("Startup")
    end
    alert_REPL!()
end

const comp_task = Ref{Task}()
const init_error = Ref{Any}()

using PrecompileTools

include("dev_packages.jl")
include("revise.jl")

@setup_workload let home = ENV["HOME"]
    _activate() = begin
        cd(dirname(dirname(pathof(Startup))))
        Pkg.activate(".", io=Base.devnull)
        __init__()
    end
    @compile_workload begin
        using Pkg: Pkg as Pkg
        using Revise
        using Alert
        if omr_enabled[]
            using OhMyREPL: JLFzf
            using OhMyREPL.JLFzf: fzf_jll
            using OhMyREPL.BracketInserter.Pkg.API.Operations.Registry: FileWatching
            using JuliaSyntax: JuliaSyntax
            include("precompile.jl")
            _activate()
            # revise!(false)
        else
            using JLLWrappers
            using JLFzf: fzf_jll
            include("fzf.jl")
            include("precompile_small.jl")
            _activate()
        end
        push!(Revise.dont_watch_pkgs, :Startup)
    end
end

export display!, @show!, @keys, deletehistory!, debug!, compilepkg
export revise!, comp_task, init_error, Revise, includet
export @alert

end
