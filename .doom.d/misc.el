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

;; ranger doesn't show hidden files by default
(setq ranger-show-hidden t)

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
;; this makes sense only if we check on each frame creation
;; (if (and (display-graphic-p) (equal (pgtk-backend-display-class) "GdkWaylandDisplay"))
;; just check for env vars and run once per server start
(if (and (getenv "WSLENV") (getenv "WAYLAND_DISPLAY"))
    (progn
      (setq wl-copy-process nil)
      (defun wl-copy (text)
        (setq wl-copy-process (make-process :name "wl-copy"
                                            :buffer nil
                                            ;; :command '("wl-copy" "-f" "-n")
                                            :command '("wex" "clip.exe")
                                            :connection-type 'pipe))
        (process-send-string wl-copy-process text)
        (process-send-eof wl-copy-process))
      (defun wl-paste ()
        (if (and wl-copy-process (process-live-p wl-copy-process))
            nil ; should return nil if we're the current paste owner
          ;; (shell-command-to-string "wl-paste -n | tr -d '\r'")
          (shell-command-to-string "wex pbpaste.exe | tr -d '\r'")
          ))
      (native-compile #'wl-copy)
      (native-compile #'wl-paste)
      (setq interprogram-cut-function #'wl-copy)
      (setq interprogram-paste-function #'wl-paste)
      ;; use custom temporary directory with WSL since there are permission problems with /tmp
      ;; NOTE: ensure trailing slash /
      (setq temporary-file-directory "/run/upper/")
      ))
;; use wslview as program (TODO: check wslu utils is installed in wsl doom PR)
(setq browse-url-generic-program (cond ((executable-find "wslview"))
                                       ((executable-find "firefox"))))
(use-package! weechat
  :commands weechat-connect)

;; use xclip mode with WSL and X11
;; (if (and (getenv "WSLENV") (getenv "DISPLAY") (equal (getenv "GDK_BACKEND") "x11"))
;;     (xclip-mode t))
;; ... actually don't if you want to paste from an emacs buffer to windows

;; not prompt for vterm compilation
(setq vterm-always-compile-module t)
