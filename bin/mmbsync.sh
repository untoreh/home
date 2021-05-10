#!/bin/bash -li
# use an interactive shell to source the ssh-agent env through fish ssh-agent plugin
# since mbsync needs access to it for decrypting mails secrets

MailDir=~/.mail
MailsList=$(find $MailDir -maxdepth 1 -name '*@*' -printf '%f\n')
MailsCount=$(echo "$MailsList" | wc -l)

# parallel --bar --progress mbsync ::: ${MailsList}
echo $MailsList | xargs -n1 -P $MailsCount mbsync
# echo "$MailsList" | rush --propagate-exit-status=false mbsync {}
