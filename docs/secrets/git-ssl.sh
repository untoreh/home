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

function decrypt(){
	openssl $CIPHER -d -iter 10000 -kfile <(echo $PASSWORD) -in /dev/stdin -out -
}

case "$1" in
"smudge")
	decrypt
	;;
"diff")
	if [ -n "$2" ]; then
		cat "$2"
	else
		decrypt
	fi
	;;
"clean")
	openssl $CIPHER -iter 10000 -kfile <(echo $PASSWORD) -in /dev/stdin -out -
	;;
*)
	echo "git attribute not understood!"
	;;
esac
