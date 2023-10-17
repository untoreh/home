#!/usr/bin/fish

# use an interactive shell to source the ssh-agent env through fish ssh-agent plugin
# since mbsync needs access to it for decrypting mails secrets
set MailDir ~/.mail/
set MailsList (find $MailDir -maxdepth 1 -name '*@*' -printf '%f\n')
set MailsCount (string split " " "$MailsList" | wc -l)

# parallel --bar --progress mbsync ::: ${MailsList}
echo $MailsList | xargs -n1 -P $MailsCount mbsync
# string split " " "$MailsList" | rush --propagate-exit-status=false mbsync {}
