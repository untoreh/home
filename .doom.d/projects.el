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
        '("/tmp" "~/tmp" "~/.local" "~/.cache" ".npm" "~/.emacs.d/.local/straight/repos/")
        projectile-project-search-path '("~/dev")
        projectile-enable-caching t
        projectile-git-submodule-command
        "git submodule --quiet foreach 'echo $path' 2>/dev/null | tr '\\n' '\\0'"
        )
  (pushnew! projectile-globally-ignored-directories "~/win" ".venv" ".env" ".ipfs" ".archive" ".old")
  ;; allow project based vars
  (put 'projectile-generic-command 'safe-local-variable #'stringp))

(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

;; magit
(after! magit
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

  (defun magit-toggle-large-repo ()
    " Toggle the large repo flag for the current repository "
    (interactive)
    (setf magit-large-repo-p
          (not (bound-and-true-p magit-large-repo-p)))
    (message "%s large repo for repository at %s"
             (if magit-large-repo-p "Enabled" "Disabled") default-directory))

  (map! :mode magit-status-mode
        "C-l" #'magit-toggle-large-repo)

  (defun magit-auto-detect-large-repo ()
    (if (and (not magit-large-repo-set-p)
             (derived-mode-p 'magit-mode))
        (let ((numfiles (string-to-number
                         (shell-command-to-string "fd . | wc -l"))))
          (save-excursion
            (when (> numfiles magit-large-repo-num-files)
              (mapc (lambda (args) (apply 'add-dir-local-variable args))
                    '((nil enable-local-eval t)
                      (nil magit-refresh-buffers nil)
                      (nil magit-large-repo-p t)
                      (nil magit-commit-show-diff nil)
                      ;; this doesn't seem to work so we just use advices
                      ;; (magit-status-mode eval
                      ;;                    (mapc
                      ;;                     'magit-disable-section-inserter
                      ;;                     '('magit-insert-staged-changes
                      ;;                       'magit-insert-unstaged-changes)))
                      )))
            (add-dir-local-variable nil 'magit-large-repo-set-p t)
            (doom/save-and-kill-buffer)))))

  (add-hook 'magit-pre-refresh-hook #'magit-auto-detect-large-repo)
  (defadvice! magit-skip-diff-large-repo (func) :around #'magit-commit-diff
    (message "is it large? %s" magit-large-repo-p)
    (unless magit-large-repo-p
      (funcall func)))
  (defadvice! magit-skip-unstaged-large-repo (func)
    :around '(magit-insert-unstaged-changes magit-insert-staged-changes)
    (unless magit-large-repo-p
      (funcall func)))
)

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
                  (run-with-idle-timer 5 t
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

;(add-hook 'emacs-startup-hook #'my/restore-session)
