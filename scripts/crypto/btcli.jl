using Pkg
if Pkg.project().name !== "BacktestCLI"
	Pkg.activate(joinpath("$(ENV["HOME"])", "tmp", "Backtest.jl", "cli"))
	Pkg.instantiate()
end

using BacktestCLI
BacktestCLI.command_main()
