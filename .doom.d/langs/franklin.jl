using Revise
using Conda
using Pkg: project

# These must be set before initializing Franklin
py_v = chomp(String(read(`$(joinpath(Conda.BINDIR, "python")) -c "import sys; print(str(sys.version_info.major) + '.' + str(sys.version_info.minor))"`)))
ENV["PYTHON3"] = joinpath(Conda.BINDIR, "python")
ENV["PIP3"] = joinpath(Conda.BINDIR, "pip")
ENV["PYTHONPATH"] = "$(Conda.LIBDIR)/python$(py_v)"

using Franklin; const fr = Franklin;
using Franklin: convert_md, convert_html, pagevar, path, globvar;

function setup_franklin()
    fr.FOLDER_PATH[] = dirname(@__FILE__)
    if isempty(fr.FOLDER_PATH[]) fr.FOLDER_PATH[] = dirname(project().path) end
    fr.set_paths!()
    fr.def_GLOBAL_VARS!()
    fr.process_config()
    # fr.fd_setup()
end

setup_franklin()

include("utils.jl")

# frank_task = @task serve(prerender=true); schedule(frank_task)
