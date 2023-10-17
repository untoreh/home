using REPL
using REPL.Terminals
using ProfileView

function start_repl()
  term = Terminals.TTYTerminal(get(ENV, "TERM", @static Sys.iswindows() ? "" : "dumb"), stdin, stdout, stderr)
  active_repl = REPL.LineEditREPL(term, true)
  REPL.run_repl(active_repl, backend-> (global active_repl_backend = backend))
end

function dosave()
  li, lidict = Profile.retrieve()
end

atexit(dosave)
Base.exit_on_sigint(false)

p = try
@profview start_repl()
catch
end

display(p)

read(stdin)
