#!/bin/bash -x
# shell script to prepend i3status with more stuff

i=0
while read line ; do
    #upsdata=`sudo chroot --userspec=fra:fra /host /bin/bash -c "upsc nutdev1"`
    upsdata="`ssh localhost upsc nutdev1`"
    ups_max_l=`echo "$upsdata" | grep realpower\.nominal | grep -o "[0-9]*"`
    ups_cur_l=`echo "$upsdata" | grep "load" | grep -o "[0-9]*"`
    if [ -z "$upsdata" ] ; then
        echo $line
        continue
    fi
    ups_p=$(($(( $ups_max_l / 100)) * $ups_cur_l))
    ups="[{\"name\":\"ups\",\"markup\":\"none\",\"full_text\":\"power: ${ups_p} W \"}]"
    line=`echo $line | sed 's/^,\(.*\)/\1/'`
    continue
    if [ $i -gt 2 ]
	  then
        echo ","`echo "$ups $line" | jq -sc add`
	  else
        if [ $i -gt 1 ]
	      then
            echo "$ups $line" | jq -sc add | sed 's/^,\(.*\)/\1/'
	      else
            echo "$line" | sed 's/^,\(.*\)/\1/'
        fi
	  fi
    i=`expr $i + 1`
done < <(/usr/bin/i3status)
