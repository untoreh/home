## server nat on fcph
88.198.69.215 19021 (external ssh port)
payload: ~10000 on port 80/443
payload: 1292 (+16 for headers) on higher ports
fling_port=19001
sync_port=19002
conns=12

## on phone
start-tcs-client
`ssh root@127.0.0.1 -p 6000`
