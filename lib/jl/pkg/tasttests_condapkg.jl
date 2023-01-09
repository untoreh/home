# All the packages added in the test/Project.toml go here (before the NO_TMP switch)
using Aqua
using Test
# Disable TMP project path
NO_TMP = "JULIA_NO_TMP" ∈ keys(ENV)
if NO_TMP
    using Pkg: Pkg
    # activate main project
    root = "."
    if isnothing(Pkg.project().name) && ispath(joinpath(dirname(pwd()), "Project.toml"))
        root = ".."
    end
    Pkg.activate(root)
    @assert Pkg.project().name == "Backtest"
    if ispath(joinpath(root, ".CondaPkg", "env"))
        ENV["JULIA_CONDAPKG_OFFLINE"] = :yes
    end
end
