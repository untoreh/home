#!/bin/bash
#
if [ -z "$PASSWORD" ]; then
	if [ -f ~/.secrets/pass ]; then
		PASSWORD=$(<~/.secrets/pass)
	else
		echo export a \$PASSWORD as recipient
		exit 1
	fi
fi
[ -z "$CIPHER" ] && CIPHER=aes128

case "$1" in
"smudge")
	openssl $CIPHER -d -iter 10000 -kfile <(echo $PASSWORD) -in /dev/stdin -out -
	;;
"diff")
	cat "$2"
	;;
"clean")
	openssl $CIPHER -iter 10000 -kfile <(echo $PASSWORD) -in /dev/stdin -out -
	;;
*)
	echo "git attribute not understood!"
	;;
esac