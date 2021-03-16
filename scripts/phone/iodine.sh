#!/system/bin/sh

## domain holding the ns record that points to the nameserver
NS_DOMAIN=${NS_DOMAIN:-localhost}
## put to direct dns ns_domain ip if custom dns are allowed
DNS=
PASS=pass
## -r disables raw udp because direct udp access to tunnel dns is required
iodine -r -P $PASS -T TXT -ORaw -I3 $DNS $NS_DOMAIN

## on server run
## iodined -c -P $PASS -d tap0 192.168.233.1/24 $NS_DOMAIN
## an easy socks5 with glider
## glider -listen socks5://192.168.233.1:1080
