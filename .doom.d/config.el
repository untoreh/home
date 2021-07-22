;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "vars")
(load! "misc")
(load! "bindings")
(load! "theme")
(load! "text")
(load! "org")
(load! "mail")
(load! "completion")
(load! "langs/langs")

(message "Configuration Loaded Succesfully!")

(let* ((session-name "main")
       (session-file (concat (file-name-as-directory (doom-session-file))
           session-name)))
  (message "restoring %s session" session-name)
  (file-readable-p session-file)
  (doom-load-session session-file))
