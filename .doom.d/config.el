;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(let ((force-load-messages t))
  (load! "vars")
  (load! "functions")
  (load! "misc")
  (load! "bindings")
  (load! "theme")
  (load! "text")
  (load! "org")
  (load! "mail")
  (load! "completion")
  (load! "langs/langs")
  (load! "projects")
  (after! magit
    (load! "git"))
  ;; (load! "maths.el")
  ;; (load! "debug")
  )

(message "Configuration Loaded Successfully!")
