;;; projects.el -*- lexical-binding: t; -*-
;;;
;; remove winner-ring from doom persp-mode because it doesn't seem to be working
;; TODO: check workspaces module for persp-mode -> eyebrowse update
(add-hook
 'doom-after-init-modules-hook
 (lambda ()
   (delete '(winner-ring . t) window-persistent-parameters)
   (remove-hook 'persp-before-deactivate-functions #'+workspaces-save-winner-data-h)
   (remove-hook 'persp-activated-functions #'+workspaces-load-winner-data-h)))

;; projectile
(after! projectile
  (setq projectile-ignored-projects
        `("/tmp" "~/tmp" "~/.local" "~/.cache" ".npm" "~/.emacs.d/.local/straight/repos/" ,(getenv "HOME"))
        projectile-project-search-path '("~/dev")
        projectile-enable-caching t
        projectile-git-submodule-command
        "git submodule --quiet foreach 'echo $path' 2>/dev/null | tr '\\n' '\\0'"
        ;; Don't consider $HOME as project
        projectile-project-root-files-bottom-up (remove ".git" projectile-project-root-files-bottom-up)
        projectile-compile-use-comint-mode t ;; fixes some scroll issues
        )
  (pushnew! projectile-globally-ignored-directories "~/win" ".venv" ".env" ".ipfs" ".archive" ".old" "node_modules")
  ;; allow project based vars
  (put 'projectile-generic-command 'safe-local-variable #'stringp)
  ;; prefer top-down to bottom up since we VC the home directory
  (setq projectile-project-root-functions
        (delete #'projectile-root-bottom-up projectile-project-root-functions))
  (nconc projectile-project-root-functions '(projectile-root-bottom-up)))

(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))


(put 'projectile-generic-command 'safe-local-variable #'stringp)

(defun my/session-file (session-name)
  (concat (f-dirname (doom-session-file))
	  "/" session-name))

;; TODO: this should be handled better by the workspace module
(defun my/delete-numbered-workspaces ()
  (mapc (lambda (name)
          (when (string-match-p "#[0-9]" name)
            (+workspace/delete name)))
        (+workspace-list-names)))

(defvar doom-session-timer (timer-create) "timer for saving doom sessions")
(defvar my/last-saved-session-time (current-time))

(defun my/toggle-session-timer (&optional session-name disable)
  (interactive)
  (let* ((session-name (or session-name "main"))
         (create-lockfiles t)
         (session-file (my/session-file session-name)))
    (if (and
         (not disable)
         (not (file-locked-p session-file)))
        (progn
          (lock-file session-file)
          (when (file-locked-p session-file)
            (when (boundp 'doom-session-timer) (cancel-timer doom-session-timer))
            (setq doom-session-timer
                  (run-with-idle-timer
                   5 t
		   (lambda () (let ((inhibit-message t))
                                (when (and (> (float-time (time-since
                                                           my/last-saved-session-time))
                                              300)
                                           (doom/save-session session-file))
                                  (setq my/last-saved-session-time (current-time)))))))))
      (progn
        (when (boundp 'doom-session-timer)
          (cancel-timer doom-session-timer)
          (makunbound 'doom-session-timer))
        (unlock-file session-file)))))

(defun my/restore-session (&optional session-name)
  ;; (toggle-debug-on-error t)
  (unless session-name
    (setq session-name "main"))
  (let ((file (my/session-file session-name)))
    (when (and (file-readable-p file) (doom/load-session file))
      (my/delete-numbered-workspaces))
    ;; enable saving session
    (my/toggle-session-timer session-name)
    ;; ensure we disable possible previous session timers before loading a new session
    (add-transient-hook! #'doom/load-session
      (lambda (file) (my/toggle-session-timer (f-base file) t)))
    ))

;; save magit buffers
;; this doesn't work because problems with lexical scope
;; https://github.com/hlissner/doom-emacs/issues/3558
;; (after! (persp-mode magit)
;;   :if (and nil (modulep! :ui workspaces))
;;   :config
;;   (persp-def-buffer-save/load
;;    :mode 'magit-status-mode :tag-symbol 'def-magit-status-buffer
;;    :save-vars '(major-mode default-directory)
;;    :after-load-function (lambda (b &rest _)
;;                           (with-current-buffer b (magit-refresh)))))


(defvar my/previous-project-root nil "The previously known (projectile) project root.")

(defun my/set-project-dir () (let ((root (projectile-project-root)))
                               (when (not (eq root my/previous-project-root))
                                 (setenv "PROJECT_DIR" root)
                                 (setq my/previous-project-root root))))

;; update `PROJECT_DIR' env var when switching projects
(add-hook! 'doom-switch-buffer-hook #'my/set-project-dir)

(defun my/set-config-name ()
  (interactive)
  ;; General purpose config variable for project
  (setenv "CONFIG_NAME" (read-string "Set CONFIG_NAME env var: ")))

(map! :leader
      :desc "Set config name"
      :nvi "p l c" #'my/set-config-name
      )
;; (add-hook 'emacs-startup-hook #'my/restore-session)

(after! envrc
  (defvar envrc-auto-reload-paths '())
  (add-hook! envrc-file-mode
    (if (member (file-truename buffer-file-name) envrc-auto-reload-paths)
        (progn
          (add-hook! 'after-save-hook :local
            (envrc-allow)
            (envrc-reload-all))))))
