# print colored ouput
function cyan
    if [ $argv[1] = "-n" ]
        echo -n (set_color cyan)$argv[2..-1](set_color normal)
    else
        echo (set_color cyan)$argv(set_color normal)
    end
end

function red
    if [ $argv[1] = "-n" ]
        echo -n (set_color red)$argv[2..-1](set_color normal)
    else
        echo (set_color red)$argv(set_color normal)
    end
end

function yellow
    if [ $argv[1] = "-n" ]
        echo -n (set_color yellow)$argv[2..-1](set_color normal)
    else
        echo (set_color yellow)$argv(set_color normal)
    end
end

function white
    if [ $argv[1] = "-n" ]
        echo -n (set_color white)$argv[2..-1](set_color normal)
    else
        echo (set_color white)$argv(set_color normal)
    end
end

function green
    if [ $argv[1] = "-n" ]
        echo -n (set_color green)$argv[2..-1](set_color normal)
    else
        echo (set_color green)$argv(set_color normal)
    end
end
