;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ;; HOTFIX
;; (defalias 'compat-string-width 'string-width)


(setq enabled-langs '(python rust julia racket raku json markdown org emacs-lisp))
(let ((force-load-messages t))
  (load! "vars")
  (load! "functions")
  (load! "misc")
  (load! "bindings")
  (load! "theme")
  (load! "text")
  (load! "org/org")
  (load! "mail")
  (load! "completion/init")
  (load! "langs/langs")
  (load! "projects")
  (after! magit
    (load! "git"))
					;(load! "maths.el")
					;(load! "debug")
  )

(message "Configuration Loaded Successfully!")
