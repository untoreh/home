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
  (if (file-readable-p session-file)
      (doom-load-session session-file))
  ;; enable saving session
  (setq doom-main-session-timer
        (run-at-time (current-time) 300
                     (lambda () (doom/save-session session-name)))))
