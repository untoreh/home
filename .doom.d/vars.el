;;; ../../../var/home/fra/.doom.d/vars.el -*- lexical-binding: t; -*-
(add-load-path!
 (concat
  (file-name-directory
   (expand-file-name
    (executable-find "mu")))
  "/../share/emacs/site-lisp/mu4e")
 "vendor/")

;; FIXME: `add-load-path!' doesn't accept lists...
(let ((default-directory (concat doom-private-dir "vendor/")))
  (normal-top-level-add-subdirs-to-load-path))

(setq
 user-full-name "untoreh"
 user-mail-address "contact@unto.re"
 auth-sources '("~/.authinfo.gpg")
 auth-source-cache-expiry nil
 password-cache-expiry nil)

;; use battery mode when unplugged
(unless (string-match-p "^Power N/A" (battery))
  (display-battery-mode 1))

(setq org-directory "~/org/")

(setq
 undo-limit 160000
 evil-want-fine-undo t
 evil-ex-visual-char-range t
 truncate-string-ellipsis "…"
 evil-split-window-below t
 evil-vsplit-window-right t
 scroll-margin 2)

(after! which-key
  (setq which-key-idle-delay 0.33
        which-key-idle-secondary-delay 0))

(after! gcmh
  (setq gcmh-auto-idle-delay-factor 50
        gcmh-idle-delay 'auto)
  ;; (comp-defun my/gcmh-long-delay ()
  ;;             (setq gcmh-idle-delay 60))
  ;; (comp-defun my/gcmh-short-delay ()
  ;;             (setq gcmh-idle-delay 0.5))
  ;; (comp-defun
  ;;  my/gcmh-focus-delay ()
  ;;  (if (catch 'focus
  ;;        (mapc (lambda (frame)
  ;;                (when (frame-focus-state frame)
  ;;                  (throw 'focus t)))
  ;;              (frame-list))
  ;;        nil)
  ;;      (my/gcmh-long-delay)
  ;;    (my/gcmh-short-delay)))
  ;; ;; focus is broken in wslg https://github.com/microsoft/wslg/issues/44
  ;; (add-function :before after-focus-change-function #'my/gcmh-focus-delay)
  )

(setq-default docker-tramp-use-names t)


(setq-default display-line-numbers-type t)

;; langs
(setq use-jupyter nil)

(setq yas-triggers-in-field t)
(setq org-plantuml-jar-path "~/.local/bin/plantuml.jar")

;; (setq gc-cons-percentage 1
;;       gc-cons-threshold 335544320)
;; (setq gc-cons-percentage 99
;;       gc-cons-threshold 1073741824)


;; keep our folds close
(setq vimish-fold-dir (concat doom-private-dir "vimish-fold"))


;; *****************************************************************************

;; backups
(setq-default
 auto-save-default t
 auto-save-interval 80
 auto-save-timeout 16)
(setq-default
 backup-directory-alist
 `(("." . ,(concat doom-private-dir "saves/"))
   (tramp-file-name-regexp . ,(concat doom-private-dir "saves/tramp/")))
 tramp-backup-directory-alist backup-directory-alist
 backup-by-copying t                    ; don't copy symlinks
 delete-old-versions t
 kept-new-versions 7
 kept-old-versions 3
 version-control t)


;; tramp
;; (setq-default
;; tramp-default-host "xnp1"
;; tramp-default-user "root"
;; tramp-default-host nil
;; tramp-default-user nil
;; tramp-chunksize 200
;; remote-file-name-inhibit-cache nil
;; tramp-verbose 1
;; vc-ignore-dir-regexp
;; (format "%s\\|%s"
;;         vc-ignore-dir-regexp
;;         tramp-file-name-regexp
;;         "/tmp/mnt/*")
;; tramp-default-method "ssh"
;; tramp-default-method "ssh"
;; tramp-use-ssh-controlmaster-options nil
;; tramp-ssh-controlmaster-options (concat
;;                                  "-o ControlPath=~/.emacs.d/.cache/ssh/.ssh-control-%%r-%%h-%%p "
;;                                  "-o ControlMaster=auto -o ControlPersist=yes")
;; )
;; login when executing docker shells to source dotfiles
;; (push "-l" (cdr (assoc 'tramp-remote-shell-args (assoc "docker" tramp-methods))))
