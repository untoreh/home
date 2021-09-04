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


;; spell
(after! spell-fu
  ;; disable by default
  (global-spell-fu-mode -1)
  ;; default is 0.25
  (setq spell-fu-idle-delay 1.0))
;; wayland clipboard support
;; this makes sense only if we check on each frame creation
;; (if (and (display-graphic-p) (equal (pgtk-backend-display-class) "GdkWaylandDisplay"))
;; just check for env vars and run once per server start
(when (getenv "WSLENV")
  ;; shouldn't be needed since wslg 1.0.0.26
  (if (and nil (getenv "WAYLAND_DISPLAY"))
      (progn
        (setq wl-copy-process nil)
        (comp-defun wl-copy (text)
                    (setq wl-copy-process (make-process :name "wl-copy"
                                                        :buffer nil
                                                        ;; :command '("wl-copy" "-f" "-n")
                                                        :command '("wex" "clip.exe")
                                                        :connection-type 'pipe))
                    (process-send-string wl-copy-process text)
                    (process-send-eof wl-copy-process))
        (comp-defun wl-paste ()
                    ;; (if (and wl-copy-process (process-live-p wl-copy-process))
                                        ; should return nil if we're the current paste owner
                    (with-temp-buffer
                      (call-process "~/bin/wex" nil t nil "pbpaste.exe")
                      (replace-regexp-in-string "\r$" ""
                                                (buffer-substring-no-properties (point-min) (point-max))))
                    ;; )
                    )
        (setq interprogram-cut-function #'wl-copy)
        (setq interprogram-paste-function #'wl-paste)))
  ;; WSL, windows grabs M-SPC so we rebind it to F13 (from windows side)
  ;; and use that as alt leader key
  (setq doom-leader-alt-key  "<269025153>"
        doom-localleader-alt-key "<269025153> m")
  ;; (map! doom-leader-alt-key #'doom-leader)
  ;; (define-localleader-key!)
  ;; use custom temporary directory with WSL since there are permission problems with /tmp
  ;; NOTE: ensure trailing slash /
  (setq temporary-file-directory "/run/upper/"))

;; use wslview as program (TODO: check wslu utils is installed in wsl doom PR)
(setq browse-url-generic-program (cond ((executable-find "wslview"))
                                       ((executable-find "firefox"))))
(use-package! weechat
  :commands (weechat-connected-p
             weechat-connect))

;; not prompt for vterm compilation
(when (featurep! :term vterm)
  (setq vterm-always-compile-module t))

;; save magit buffers
;; this doesn't work because problems with lexical scope
;; https://github.com/hlissner/doom-emacs/issues/3558
;; (after! (persp-mode magit)
;;   :if (and nil (featurep! :ui workspaces))
;;   :config
;;   (persp-def-buffer-save/load
;;    :mode 'magit-status-mode :tag-symbol 'def-magit-status-buffer
;;    :save-vars '(major-mode default-directory)
;;    :after-load-function (lambda (b &rest _)
;;                           (with-current-buffer b (magit-refresh)))))

;; gargbace collector mode
(use-package! gcbal
  :init
  (defun gcmh-mode (&rest args))
  (defun gcmh-set-high-threshold (&rest args))
  :config
  (native-compile-async (locate-library "gcbal") t t)
  (setq
   gcbal-target-gctime 0.2
   gcbal-target-auto t)
  (gcbal-mode))
