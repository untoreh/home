;;; ../../../var/home/fra/.doom.d/vars.el -*- lexical-binding: t; -*-

(add-load-path!
 (concat
  (file-name-directory
   (expand-file-name
    (executable-find "mu")))
  "/../share/emacs/site-lisp/mu4e")
 (concat doom-user-dir "vendor/"))

;; FIXME: `add-load-path!' doesn't accept lists...
(let ((default-directory (concat doom-private-dir "vendor/")))
  (normal-top-level-add-subdirs-to-load-path))

(setq
 user-full-name "untoreh"
 user-mail-address "contact@unto.re"
 auth-sources '("~/.authinfo.gpg")
 auth-source-cache-expiry nil
 password-cache-expiry nil
 shell-file-name "/usr/bin/fish"
 )

;; use battery mode when unplugged
;; FIXME: the battery makes the modeline outflow
(let ((battery-info (battery)))
  (when (and (stringp battery-info)
             (string-match-p "^Power N/A" (battery)))
    (display-battery-mode 1)))

(setq org-directory "~/org/")

(setq
 load-prefer-newer t
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

;; shut up native comp
(setq native-comp-async-report-warnings-errors nil)

(after! gcmh
  (setq gcmh-auto-idle-delay-factor 50
        gcmh-idle-delay 'auto)
  ;; focus is broken in wslg https://github.com/microsoft/wslg/issues/44
  )

(setq-default docker-tramp-use-names t)


(setq-default display-line-numbers-type t)

;; langs
(setq use-jupyter nil)
(setq-default major-mode 'org-mode)

(setq yas-triggers-in-field t)
(setq org-plantuml-jar-path "~/.local/bin/plantuml.jar")

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

;; git
(defvar magit-large-repo-num-files 1000 "Repositories that exceed this variable are considered large.")
(defvar magit-large-repo-p nil "t if current repo is a large repo.")
(defvar magit-large-repo-set-p nil "t if current repo has been checked for largeness.")
;; allow local variables
(let ((safe-var-list '(enable-local-eval
                       magit-large-repo-p
                       magit-large-repo-set-p
                       magit-commit-show-diff
                       magit-refresh-buffers)))
  (mapc (lambda (sym)
          (put sym 'safe-local-variable (lambda (&rest args) t)))
        safe-var-list))
(let ((safe-eval-list '(mapc magit-disable-section-inserter)))
  (mapc (lambda (sym) (put sym 'safe-local-eval-function (lambda (&rest args) t)))
        safe-eval-list))
;; allow local eval
(put 'enable-local-eval 'risky-local-variable nil)


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
