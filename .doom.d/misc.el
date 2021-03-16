;;; ../../../var/home/fra/.doom.d/misc.el -*- lexical-binding: t; -*-

(use-package! emacs-async
  :commands dired-async-mode
  :config
  (dired-async-mode t))

;; Quickly close and reopen a file
(defun save-close-reopen-file ()
  (interactive)
  (save-buffer t)
  ;; buffer-file-name should always takes the true name
  ;; even in indirect buffer contexes
  (letrec ((tmp buffer-file-name))
    (if (not (eq tmp nil))
        (progn
          (kill-buffer (buffer-base-buffer (current-buffer)))
          (find-file tmp)))))

;; matrix
;(use-package! matrix-client
;  :commands matrix-client-connect)

;; dont format snippets (list is negated)
(add-to-list '+format-on-save-enabled-modes 'snippet-mode 'append)
(add-to-list '+format-on-save-enabled-modes 'web-mode 'append)
(add-to-list '+format-on-save-enabled-modes 'org-msg-edit-mode 'append)
(add-to-list '+format-on-save-enabled-modes 'gitignore-mode 'append)
(add-to-list '+format-on-save-enabled-modes 'lisp-data-mode 'append)
