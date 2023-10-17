if Base.isinteractive()
    push!(LOAD_PATH, joinpath(ENV["HOME"], ".julia", "config",  "Startup"))
    using Startup
end
t = @async begin
    while !(isdefined(Base, :active_repl))
        sleep(0.1)
    end
    @eval Main Startup.includestartup()
end
