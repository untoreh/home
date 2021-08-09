;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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

(message "Configuration Loaded Succesfully!")

(defun my/restore-main-session nil
  (let* ((session-name "main")
         (session-file (concat (f-dirname (doom-session-file))
			       "/" session-name)))
    (message "restoring %s session" session-name)
    (when (and (file-readable-p session-file) (doom-load-session session-file))
      ;; delete numbered workspaces
      (mapc (lambda (name)
              (when (string-match-p "#[0-9]" name)
                (+workspace/delete name)))
            (+workspace-list-names)))
    ;; enable saving session
    (setq doom-main-session-timer
	  (run-at-time (current-time) 300
		       (lambda () (doom/save-session session-file))))))
(add-hook 'emacs-startup-hook #'my/restore-main-session)
