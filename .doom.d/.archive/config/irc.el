;; irc
;; circe
;; (dolist (p '(circe circe-notifications))
;;   (push p dotspacemacs-additional-packages))
(setq-default circe-server-buffer-name "{network}")
(setq-default circe-network-options
              '(
                ;; ("znc/bitlbee"
                ;;  :host "znc.unto.re"
                ;;  :nick "untoreh"
                ;;  :port 6697
                ;;  :pass (lambda (_)
                ;;          (with-temp-buffer
                ;;            (insert-file-contents-literally "~/.emacs.d/private/circe-znc.el")
                ;;            (plist-get (read (buffer-string)) :znc/bitlbee)))
                ;;  :tls t
                ;;  )
                ("znc/gnome"
                 :host "znc.unto.re"
                 :nick "untoreh"
                 :pass (lambda (_)
                         (with-temp-buffer
                           (insert-file-contents-literally "~/.emacs.d/private/circe-znc.el")
                           (plist-get (read (buffer-string)) :znc/gnome)))
                 :port 6697
                 :tls t
                 )
                ("znc/gitter"
                 :host "znc.unto.re"
                 :nick "untoreh"
                 :pass (lambda (_)
                         (with-temp-buffer
                           (insert-file-contents-literally "~/.emacs.d/private/circe-znc.el")
                           (plist-get (read (buffer-string)) :znc/gitter)))
                 :lagmon-disabled t
                 :port 6697
                 :tls t
                 )
                ("znc/OFTC"
                 :host "znc.unto.re"
                 :nick "untoreh"
                 :pass (lambda (_)
                         (with-temp-buffer
                           (insert-file-contents-literally "~/.emacs.d/private/circe-znc.el")
                           (plist-get (read (buffer-string)) :znc/OFTC)))
                 :port 6697
                 :tls t
                 )
                ("znc/mozilla"
                 :host "znc.unto.re"
                 :nick "untoreh"
                 :pass (lambda (_)
                         (with-temp-buffer
                           (insert-file-contents-literally "~/.emacs.d/private/circe-znc.el")
                           (plist-get (read (buffer-string)) :znc/mozilla)))
                 :port 6697
                 :tls t
                 )
                ("znc/quakenet"
                 :host "znc.unto.re"
                 :nick "untoreh"
                 :pass (lambda (_)
                         (with-temp-buffer
                           (insert-file-contents-literally "~/.emacs.d/private/circe-znc.el")
                           (plist-get (read (buffer-string)) :znc/quakenet)))
                 :port 6697
                 :tls t
                 )
                ("znc/freenode"
                 :host "znc.unto.re"
                 :nick "untoreh"
                 :pass (lambda (_)
                         (with-temp-buffer
                           (insert-file-contents-literally "~/.emacs.d/private/circe-znc.el")
                           (plist-get (read (buffer-string)) :znc/freenode)))
                 :port 6697
                 :tls t
                 )
                ))
(setq-default
 lui-time-stamp-format "%H:%M"
 lui-time-stamp-position 'right-margin
 lui-fill-type nil
 lui-logging-directory "~/.emacs.d/private/saves/circe")
;; circe lui logging
(load "lui-logging" nil t)
;; (enable-lui-logging-globally)
;; circe lui theming
(add-hook 'lui-mode-hook 'my-lui-setup)
(defun my-lui-setup ()
  (setq
   fringes-outside-margins t
   right-margin-width 5
   word-wrap t
   wrap-prefix "    ")
  (circe-lagmon-mode t)
  (linum-mode 0)
  )
(setf (cdr (assoc 'continuation fringe-indicator-alist)) nil)
;; circe notifications
(autoload 'enable-circe-notifications "circe-notifications" nil t)

(eval-after-load "circe-notifications"
  '(progn (setq-default circe-notifications-watch-strings
                        '("untoreh"))
          (setq-default tracking-buffer
                        nil)))

(add-hook 'circe-server-connected-hook 'enable-circe-notifications)

;; (setq irccc t) ;; startup disable
(if (boundp 'irccc)
    ;; then
    'nil
  ;; else
  (circe "znc/freenode")
  (circe "znc/OFTC")
  ;; (circe "Gitter")
  (setq-default irccc t))
