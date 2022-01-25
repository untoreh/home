import Pkg
Pkg.add("DaemonMode")
using DaemonMode
shared=get(ENV, "JULIA_DAEMON_SHARED", "false") |> Meta.parse |> Bool
serve(3000, shared)
