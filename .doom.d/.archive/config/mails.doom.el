;;; ../../../var/home/fra/.doom.d/private/config/mails.doom.el -*- lexical-binding: t; -*-

(set-email-account! "a-bassomails@gmail.com"
                    '((mu4e-sent-folder       . "/bassomails@gmail.com/\[Gmail\]/Sent Mail")
                      (mu4e-drafts-folder     . "/bassomails@gmail.com/\[Gmail\]/Drafts")
                      (smtpmail-smtp-user     . "bassomails@gmail.com")
                      (user-mail-address      . "bassomails@gmail.com")
                      (mu4e-compose-signature . "")
                      (smtpmail-smtp-user . "bassomails@gmail.com")
                      (smtpmail-smtp-server . "smtp.gmail.com")
                      (smtpmail-stream-type . ssl)
                      (smtpmail-smtp-service . 465))
                    t)
(set-email-account! "b-bassobassista@gmail.com"
                    '((mu4e-sent-folder       . "/bassobassista@gmail.com/\[Gmail\]/Sent Mail")
                      (mu4e-sent-folder       . "/bassobassista@gmail.com/\[Gmail\]/Draft")
                      (smtpmail-smtp-user     . "bassobassista@gmail.com")
                      (user-mail-address      . "bassobassista@gmail.com")
                      (mu4e-compose-signature . "")
                      (smtpmail-smtp-user . "bassobassista@gmail.com")
                      (smtpmail-smtp-server . "smtp.gmail.com")
                      (smtpmail-stream-type . ssl)
                      (smtpmail-smtp-service . 465))
                    t)
(set-email-account! "c-frafragiannelli@gmail.com"
                    '((mu4e-sent-folder       . "/frafragiannelli@gmail.com/\[Gmail\]/Sent Mail")
                      (mu4e-drafts-folder     . "/frafragiannelli@gmail.com/\[Gmail\]/Drafts")
                      (smtpmail-smtp-user     . "frafragiannelli@gmail.com")
                      (user-mail-address      . "frafragiannelli@gmail.com")
                      (mu4e-compose-signature . "")
                      (smtpmail-smtp-user . "frafragiannelli@gmail.com")
                      (smtpmail-smtp-server . "smtp.gmail.com")
                      (smtpmail-stream-type . ssl)
                      (smtpmail-smtp-service . 465))
                    t)
(set-email-account! "d-criptafra@gmail.com"
                    '((mu4e-sent-folder       . "/criptafra@gmail.com/\[Gmail\]/Sent Mail")
                      (mu4e-drafts-folder     . "/criptafra@gmail.com/\[Gmail\]/Drafts")
                      (smtpmail-smtp-user     . "criptafra@gmail.com")
                      (user-mail-address      . "criptafra@gmail.com")
                      (mu4e-compose-signature . "")
                      (smtpmail-smtp-user . "criptafra@gmail.com")
                      (smtpmail-smtp-server . "smtp.gmail.com")
                      (smtpmail-stream-type . ssl)
                      (smtpmail-smtp-service . 465))
                    t)
(set-email-account! "e-francesco.giannelli@yahoo.it"
                    '((mu4e-sent-folder       . "/francesco.giannelli@yahoo.it/Sent")
                      (mu4e-drafts-folder     . "/francesco.giannelli@yahoo.it/Drafts")
                      (smtpmail-smtp-user     . "francesco.giannelli@yahoo.it")
                      (user-mail-address      . "francesco.giannelli@yahoo.it")
                      (mu4e-compose-signature . "")
                      (smtpmail-smtp-user . "francesco.giannelli@yahoo.it")
                      (smtpmail-smtp-server . "smtp.mail.yahoo.com")
                      (smtpmail-stream-type . ssl)
                      (smtpmail-smtp-service . 465))
                    t)
(set-email-account! "f-bassobassista@yahoo.it"
                    '((mu4e-sent-folder       . "/bassobassista@yahoo.it/Sent")
                      (mu4e-drafts-folder     . "/bassobassista@yahoo.it/Drafts")
                      (smtpmail-smtp-user     . "bassobassista@yahoo.it")
                      (user-mail-address      . "bassobassista@yahoo.it")
                      (mu4e-compose-signature . "")
                      (smtpmail-smtp-user . "bassobassista@yahoo.it")
                      (smtpmail-smtp-server . "smtp.mail.yahoo.com")
                      (smtpmail-stream-type . ssl)
                      (smtpmail-smtp-service . 465))
                    t)
(set-email-account! "g-bassobassista@hotmail.it"
                    '((mu4e-sent-folder       . "/bassobassista@hotmail.it/Sent")
                      (mu4e-drafts-folder     . "/bassobassista@hotmail.it/Drafts")
                      (mu4e-refile-folder     . "/bassobassista@hotmail.it/All Mail")
                      (smtpmail-smtp-user     . "bassobassista@hotmail.it")
                      (user-mail-address      . "bassobassista@hotmail.it")
                      (mu4e-compose-signature . "")
                      (smtpmail-smtp-user . "bassobassista@hotmail.it")
                      (smtpmail-smtp-server . "smtp.live.com")
                      (smtpmail-stream-type . starttls)
                      (smtpmail-smtp-service . 587))
                    t)

(setq-default
              mu4e-maildir "~/.mail"
              mu4e-trash-folder "/Trash"
              mu4e-refile-folder "/Archive"
              mu4e-get-mail-command "~/.bin/mmbsync"
              mu4e-update-interval 900
              mu4e-compose-signature-auto-include nil
              mu4e-view-show-images t
              mu4e-view-show-addresses t
              )

;; Bookmarks
(setq-default mu4e-bookmarks
              `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
                ("date:today..now" "Today's messages" ?t)
                ("date:7d..now" "Last 7 days" ?w)
                ("date:30d..now" "Last 30 days" ?m)
                ("mime:image/*" "Messages with images" ?p)
                (,(mapconcat 'identity
                             (mapcar
                              (lambda (maildir)
                                (concat "maildir:" (car maildir)))
                              mu4e-maildir-shortcuts) " OR ")
                 "All inboxes" ?i)))
(after! mu4e-alert
  (mu4e-alert-set-default-style 'notifications))
