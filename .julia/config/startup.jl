if Base.isinteractive()
    push!(LOAD_PATH, joinpath(ENV["HOME"], ".julia", "config",  "Startup"))
    using Startup
end
Startup.includestartup()
