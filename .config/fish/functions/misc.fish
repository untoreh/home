# ## colors
function cocat
    cat "$argv" | colorize_via_pygmentize
end
function colpick
    grim -g "(slurp)" -t png - | \
        convert - -format '%[pixel:p{0,0}]' txt:-
end

function zsnp
    sudo zfs snapshot rpool/ROOT/ubuntu@(date +%m-%d-%y)
end

function tabul
    cat $1 | sed -r 's/\s+/ /g' | column -t -s' '
end
function symf
    cp --remove-destination (readlink $argv[1]) $argv[1]
end
function wtr
    set -q argv[1]
    or set argv[1] "cassano delle murge"
    set -l params (echo $argv[1] | sed 's/ /%20/g')
    curl "https://en.wttr.in/$params"
end

function enit
    translate -s en -t it $argv
end

function iten
    translate -s it -t en $argv
end
