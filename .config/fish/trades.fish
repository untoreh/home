function ratesx
    curl "$argv[3].rate.sx/$argv[1]$argv[2]"
end

function btrsmp
    ssh mbx jle ~/scripts/crypto/btcli.jl resample $argv 2>/dev/null
    test $status -eq 0 && ssh mbx /usr/bin/rsync -r ~/.cache/Backtest.jl/data home:~/.cache/Backtest.jl/data 2>/dev/null
end
