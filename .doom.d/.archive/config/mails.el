;; mail

(with-eval-after-load 'mu4e
  (setq-default mu4e-contexts
                `(,(make-mu4e-context
                    :name "a-bassomails@gmail.com"
                    :enter-func (lambda () (mu4e-message "bassomails@gmail.com"))
                    ;; leave-func not defined
                    :match-func (lambda (msg)
                                  (when msg
                                    (mu4e-message-contact-field-matches msg
                                                                        :to "bassomails@gmail.com")))
                    :vars '(  ( user-mail-address      . "bassomails@gmail.com"  )
                              ( user-full-name     . "Fra Gia" )
                              ( mu4e-compose-signature .
                                                       (concat
                                                        "Fra Gia\n"
                                                        "Cassano delle Murge, Italia\n"))
                              (mu4e-sent-messages-behavior . delete)
                              (mu4e-sent-folder . "/bassomails@gmail.com/\[Gmail\]/Sent Mail")
                              (mu4e-drafts-folder . "/bassomails@gmail.com/\[Gmail\]/Drafts")
                              (smtpmail-smtp-user . "bassomails@gmail.com")
                              (smtpmail-smtp-server . "smtp.gmail.com")
                              (smtpmail-stream-type . ssl)
                              (smtpmail-smtp-service . 465)))
                  ,(make-mu4e-context
                    :name "b-bassobassista@gmail.com"
                    :enter-func (lambda () (mu4e-message "bassobassista@gmail.com"))
                    ;; leave-fun not defined
                    :match-func (lambda (msg)
                                  (when msg
                                    (mu4e-message-contact-field-matches msg
                                                                        :to "bassobassista@gmail.com")))
                    :vars '(  ( user-mail-address      . "bassobassista@gmail.com" )
                              ( user-full-name     . "Fra Gia" )
                              (mu4e-sent-messages-behavior . delete)
                              (mu4e-sent-folder . "/bassobassista@gmail.com/\[Gmail\]/Sent Mail")
                              (mu4e-drafts-folder . "/bassobassista@gmail.com/\[Gmail\]/Drafts")
                              (smtpmail-smtp-user . "bassobassista@gmail.com")
                              (smtpmail-smtp-server . "smtp.gmail.com")
                              (smtpmail-stream-type . ssl)
                              (smtpmail-smtp-service . 465)))
                  ,(make-mu4e-context
                    :name "c-frafragiannelli@gmail.com"
                    :enter-func (lambda () (mu4e-message "frafragiannelli@gmail.com"))
                    ;; leave-fun not defined
                    :match-func (lambda (msg)
                                  (when msg
                                    (mu4e-message-contact-field-matches msg
                                                                        :to "frafragiannelli@gmail.com")))
                    :vars '(  ( user-mail-address      . "frafragiannelli@gmail.com" )
                              ( user-full-name     . "Fra Gia" )
                              (mu4e-sent-messages-behavior . delete)
                              (mu4e-sent-folder . "/frafragiannelli@gmail.com/\[Gmail\]/Sent Mail")
                              (mu4e-drafts-folder . "/frafragiannelli@gmail.com/\[Gmail\]/Drafts")
                              (smtpmail-smtp-user . "frafragiannelli@gmail.com")
                              (smtpmail-smtp-server . "smtp.gmail.com")
                              (smtpmail-stream-type . ssl)
                              (smtpmail-smtp-service . 465)))
                  ,(make-mu4e-context
                    :name "d-criptafra@gmail.com"
                    :enter-func (lambda () (mu4e-message "criptafra@gmail.com"))
                    ;; leave-fun not defined
                    :match-func (lambda (msg)
                                  (when msg
                                    (mu4e-message-contact-field-matches msg
                                                                        :to "criptafra@gmail.com")))
                    :vars '(  ( user-mail-address      . "criptafra@gmail.com" )
                              ( user-full-name     . "Fra Gia" )
                              (mu4e-sent-messages-behavior . delete)
                              (mu4e-sent-folder . "/criptafra@gmail.com/\[Gmail\]/Sent Mail")
                              (mu4e-drafts-folder . "/criptafra@gmail.com/\[Gmail\]/Drafts")
                              (smtpmail-smtp-user . "criptafra@gmail.com")
                              (smtpmail-smtp-server . "smtp.gmail.com")
                              (smtpmail-stream-type . ssl)
                              (smtpmail-smtp-service . 465)))
                  ,(make-mu4e-context
                    :name "e-francesco.giannelli@yahoo.it"
                    :enter-func (lambda () (mu4e-message "francesco.giannelli@yahoo.it"))
                    ;; leave-fun not defined
                    :match-func (lambda (msg)
                                  (when msg
                                    (mu4e-message-contact-field-matches msg
                                                                        :to "francesco.giannelli@yahoo.it")))
                    :vars '(  ( user-mail-address      . "francesco.giannelli@yahoo.it" )
                              ( user-full-name     . "Francesco Giannelli" )
                              (mu4e-sent-messages-behavior . delete)
                              (mu4e-sent-folder . "/francesco.giannelli@yahoo.it/Sent")
                              (mu4e-drafts-folder . "/francesco.giannelli@yahoo.it/Drafts")
                              (smtpmail-smtp-user . "francesco.giannelli@yahoo.it")
                              (smtpmail-smtp-server . "smtp.mail.yahoo.com")
                              (smtpmail-stream-type . ssl)
                              (smtpmail-smtp-service . 465)))
                  ,(make-mu4e-context
                    :name "f-bassobassista@yahoo.it"
                    :enter-func (lambda () (mu4e-message "bassobassista@yahoo.it"))
                    ;; leave-fun not defined
                    :match-func (lambda (msg)
                                  (when msg
                                    (mu4e-message-contact-field-matches msg
                                                                        :to "bassobassista@yahoo.it")))
                    :vars '(  ( user-mail-address      . "bassobassista@yahoo.it" )
                              ( user-full-name     . "Fra Gia" )
                              (mu4e-sent-messages-behavior . delete)
                              (mu4e-sent-folder . "/bassobassista@yahoo.it/Sent")
                              (mu4e-drafts-folder . "/bassobassista@yahoo.it/Drafts")
                              (smtpmail-smtp-user . "bassobassista@yahoo.it")
                              (smtpmail-smtp-server . "smtp.mail.yahoo.com")
                              (smtpmail-stream-type . ssl)
                              (smtpmail-smtp-service . 465)))
                  ,(make-mu4e-context
                    :name "g-bassobassista@hotmail.it"
                    :enter-func (lambda () (mu4e-message "bassobassista@hotmail.it"))
                    ;; leave-fun not defined
                    :match-func (lambda (msg)
                                  (when msg
                                    (mu4e-message-contact-field-matches msg
                                                                        :to "bassobassista@hotmail.it")))
                    :vars '(  ( user-mail-address      . "bassobassista@hotmail.it" )
                              ( user-full-name     . "Fra Gia" )
                              (mu4e-sent-messages-behavior . delete)
                              (mu4e-sent-folder . "/bassobassista@hotmail.it/Sent")
                              (mu4e-drafts-folder . "/bassobassista@hotmail.it/Drafts")
                              (smtpmail-smtp-user . "bassobassista@hotmail.it")
                              (smtpmail-smtp-server . "smtp.live.com")
                              (smtpmail-stream-type . starttls)
                              (smtpmail-smtp-service . 587)))
                  )))
(setq-default message-send-mail-function 'smtpmail-send-it)
;; better viewving
(setq-default browse-url-generic-program "~/.bin/ffox")
(setq-default browse-url-generic-args '())
(setq-default  mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
(add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser))
;; mail hooks
(add-hook 'mu4e-view-mode-hook #'visual-line-mode)
(add-hook 'mu4e-compose-mode-hook #'flycheck-mode)
(setq-default mail-user-agent 'mu4e-user-agent)
;; Set up some common mu4e variables
(setq-default mu4e-maildir "~/.mail"
              mu4e-trash-folder "/Trash"
              mu4e-refile-folder "/Archive"
              mu4e-get-mail-command "~/.bin/mmbsync"
              mu4e-update-interval 900
              mu4e-compose-signature-auto-include nil
              mu4e-view-show-images t
              mu4e-view-show-addresses t
              )


;; Mail directory shortcuts
(setq-default mu4e-maildir-shortcuts
              '(("/Inbox" . ?g)))

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
;; Enable Desktop notifications
(with-eval-after-load 'mu4e-alert
  (mu4e-alert-set-default-style 'notifications))
