;;; files.el -*- lexical-binding: t; -*-

(require 'magit)

(when (and (modulep! :ui treemacs)
           (modulep! :tools magit))
  (setq +treemacs-git-mode 'deferred))

(defun magit-toggle-large-repo ()
  " Toggle the large repo flag for the current repository "
  (interactive)
  (setf magit-large-repo-p
        (not (bound-and-true-p magit-large-repo-p)))
  (message "%s large repo for repository at %s"
           (if magit-large-repo-p "Enabled" "Disabled") default-directory))

(defun magit-auto-detect-large-repo (&optional force)
  "Decide if current repository has more files than `magit-large-repo-num-files'."
  (if (and (or force (not magit-large-repo-set-p))
           (derived-mode-p 'magit-mode))
      (let ((numfiles (string-to-number
                       (shell-command-to-string "fd . | wc -l"))))
        (save-excursion
          (when (or force (> numfiles magit-large-repo-num-files))
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

;; Prevent magit functions if the `magit-large-repo-p' is t.
(add-hook 'magit-pre-refresh-hook #'magit-auto-detect-large-repo)
(defadvice! magit-skip-diff-large-repo (func) :around #'magit-commit-diff
  (message "is it large? %s" magit-large-repo-p)
  (unless magit-large-repo-p
    (funcall func)))
(defadvice! magit-skip-unstaged-large-repo (func)
  :around '(magit-insert-unstaged-changes magit-insert-staged-changes)
  (unless magit-large-repo-p
    (funcall func)))

(defun magit-insert-funding ()
  "Add funding file to a (github) git repository."
  (interactive)
  (let ((content "github: # Replace with up to 4 GitHub Sponsors-enabled usernames e.g., [user1, user2]
liberapay: untoreh
custom: ['https://paypal.me/untoreh']")
        (gh-dir (expand-file-name ".github/" (or (and projectile-mode
                                                      (projectile-project-root))
                                                 default-directory))))
    (with-temp-buffer
      (insert content)
      (make-directory gh-dir)
      (write-file (expand-file-name "FUNDING.yml" gh-dir)))))

(map! :mode magit-status-mode :leader
      ("g e f" #'magit-insert-funding)
      ("g e l" #'magit-toggle-large-repo))
