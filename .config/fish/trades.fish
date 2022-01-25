alias bfqt "ssh -t vrmc6 tmux a -t freqtrade"
alias vfqt "ssh -t vrmc6 fqt"

function ratesx
    curl "$argv[3].rate.sx/$argv[1]$argv[2]"
end

set FQT_DIR ~/.tmp/freqtrade
alias fqtt "$FQT_DIR/scripts/exec.sh bash -li"
alias qclean "dkr run -v ~/.tmp/lean_user:/lean_user --net host -it --name lean -w /Lean --entrypoint '/bin/bash' quantconnect/lean -li"

function btrsmp
    ssh mbx jle ~/scripts/crypto/btcli.jl resample $argv 2>/dev/null
    test $status -eq 0 && ssh mbx /usr/bin/rsync -r ~/.cache/Backtest.jl/data home:~/.cache/Backtest.jl/data 2>/dev/null
end
