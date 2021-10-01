using Revise
using Conda

# These must be set before initializing Franklin
py_v = chomp(String(read(`$(joinpath(Conda.BINDIR, "python")) -c "import sys; print(str(sys.version_info.major) + '.' + str(sys.version_info.minor))"`)))
ENV["PYTHON3"] = joinpath(Conda.BINDIR, "python")
ENV["PIP3"] = joinpath(Conda.BINDIR, "pip")
ENV["PYTHONPATH"] = "$(Conda.LIBDIR)/python$(py_v)"

using Franklin;
const fr = Franklin;
using Franklin: convert_md, convert_html, pagevar, path, globvar;

if isdefined(Main, :frank_task)
    Base.throwto(frank_task, InterruptException())
end
include("utils.jl")

function pubup(;opt=true, search=true, trans=true)
	opt && fr.optimize(prerender=true; minify=true)
    search && lunr()
    trans && translate_website()
end

frank_task = @task serve(prerender=true); schedule(frank_task)
