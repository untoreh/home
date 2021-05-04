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
(use-package! matrix-client
  :init
  :commands matrix-client-connect)

;; dont format snippets (list is negated)
(add-to-list '+format-on-save-enabled-modes 'snippet-mode 'append)
(add-to-list '+format-on-save-enabled-modes 'web-mode 'append)
(add-to-list '+format-on-save-enabled-modes 'org-msg-edit-mode 'append)
(add-to-list '+format-on-save-enabled-modes 'gitignore-mode 'append)
(add-to-list '+format-on-save-enabled-modes 'lisp-data-mode 'append)

;; never enable indent guides by default
(remove-hook! (prog-mode text-mode conf-mode) highlight-indent-guides-mode)

;; spell
(after! spell-fu
  ;; disable by default
  (global-spell-fu-mode -1)
  ;; default is 0.25
  (setq spell-fu-idle-delay 1.0))

;; where projectile search for projects
(setq projectile-project-search-path '("~/dev"))

;; wayland clipboard support
(if (equal (pgtk-backend-display-class) "GdkWaylandDisplay")
    (progn
      (setq wl-copy-process nil)
      (defun wl-copy (text)
        (setq wl-copy-process (make-process :name "wl-copy"
                                            :buffer nil
                                            :command '("wl-copy" "-f" "-n")
                                            :connection-type 'pipe))
        (process-send-string wl-copy-process text)
        (process-send-eof wl-copy-process))
      (defun wl-paste ()
        (if (and wl-copy-process (process-live-p wl-copy-process))
            nil ; should return nil if we're the current paste owner
          (shell-command-to-string "wl-paste -n | tr -d '\r'")))
      (native-compile 'wl-copy)
      (native-compile 'wl-paste)
      (setq interprogram-cut-function 'wl-copy)
      (setq interprogram-paste-function 'wl-paste)))
