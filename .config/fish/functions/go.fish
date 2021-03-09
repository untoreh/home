
function goextdeps
    gol -f '{{.Deps}}' | \
        tr "[" " " | tr "]" " " | \
        xargs go list -f '{{if not .Standard}}{{.ImportPath}}{{end}}'
end
