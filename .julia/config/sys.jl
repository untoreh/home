import Pkg
if !isinstalled("PackageCompiler")
    Pkg.add("PackageCompiler")
end

replace_default = false
# image_path = "/opt/julia/lib/julia/sys.so.ls"
image_path = ""
if isfile("/opt/julia/lib/julia/sys.so")
    image_path = "/opt/julia/lib/julia/sys.so.ls"
else
    image_path = "/usr/lib/julia/sys.so.ls"
end

using PackageCompiler

# :JuliaFormatter
meta = [:FilePathsBase, :JSON, :JSONRPC, :URIParser, :Tokenize, :StaticLint, :Revise]
deps = [
    :Plots,
    :PyCall,
    :Compat,
    :BenchmarkTools,
    :TimeZones,
    :IterTools,
    :DataFrames,
    :TimeSeries,
    :StatsBase,
    :Transducers,
    :OnlineStats,
    :DataStructures,
    :ImageFiltering,
    :ColorSchemes,
]
ls = [:LanguageServer, :SymbolServer]
if in("JULIA_PKGS", keys(ENV))
    from_env = [Symbol(strip(p)) for p in split(ENV["JULIA_PKGS"], ENV)]
else
    from_env = Vector{Symbol}(undef, 0)
end

pkgs = vcat(deps, meta, ls, from_env)

create_sysimage(pkgs, replace_default=replace_default, sysimage_path=image_path)
