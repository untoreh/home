alias anp "ansible-playbook --sftp-extra-args='-B 512'"
alias anc "ansible-console"

function sync-proxies
    mkdir -p ~/tmp/proxies
    scp vrmc2:/opt/py/proxies.json ~/tmp/proxies/proxies.json
    ~/scripts/srv/split-proxies.py > ~/tmp/proxies/proxies.txt
end
