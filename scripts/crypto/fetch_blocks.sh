#!/bin/sh

sel=unprll

unprll=localhost:21150
nerva=localhost:17566
monero=
masari=nodes.hashvault.pro:38081
turtle=nodes.hashvault.pro:11898
eval "node=\$$sel"
blocks=2880
case "$sel" in
    "turtle") 
        height=$(curl -s -X POST -H 'Content-Type: application/json' http://$node/json_rpc -d '{"jsonrpc":"2.0","method":"getblockcount","params":{}}' | jq '.result.count')
        ;;
    *)
        height=$(curl -s -X POST http://$node/get_height | jq '.height')
        ;;
esac
start=$((height-blocks))
end=$((height-1))
curl -s -X POST \
     http://$node/json_rpc -d \
     '{"jsonrpc":"2.0","id":"0","method":"get_block_headers_range","params":{"start_height":'"$start"',"end_height":'"$end"'}}' \
     -H 'Content-Type: application/json'
