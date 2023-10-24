;;; ../../../var/home/fra/.doom.d/misc.el -*- lexical-binding: t; -*-

(use-package! emacs-async
  :defer
  :commands dired-async-mode
  :config
  (dired-async-mode t))

(map! :mode emacs-lisp-mode
      :localleader
      :desc "compile and load buffer"
      "e B" #'emacs-lisp-native-compile-and-load)

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

;; wayland clipboard support
;; this makes sense only if we check on each frame creation
;; (if (and (display-graphic-p) (equal (pgtk-backend-display-class) "GdkWaylandDisplay"))
;; just check for env vars and run once per server start
(when wslp
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
  ;; and use that as alt leader key NOTE: should be "XF86Tools" with X and "Tools" in PGTK
  (let ((k (if (boundp 'pgtk-initialized) "<Tools>" "<XF86Tools>")))
    (setq doom-leader-alt-key  k
          doom-localleader-alt-key (concat k " m")))
  ;; (map! doom-leader-alt-key #'doom-leader)
  ;; (define-localleader-key!)
  ;; use custom temporary directory with WSL since there are permission problems with /tmp
  ;; NOTE: ensure trailing slash /
  (setq temporary-file-directory "/run/upper/")sl)

;; use wslview as program (TODO: check wslu utils is installed in wsl doom PR)
(setq browse-url-generic-program
      (cond ((and wslp (executable-find "wslview")))
            ((executable-find "firefox"))))

;; not prompt for vterm compilation
(use-package! vterm
  :if (modulep! :term vterm)
  :config
  (setq vterm-always-compile-module t
        vterm-kill-buffer-on-exit nil
        ;; make vterm buffer updates a little faster
        vterm-timer-delay 0.033
        evil-collection-vterm-send-escape-to-vterm-p t
        )
  ;; this make vterm stop autosrolling after in evil normal state
  ;; however it stops the vterm process...
  ;; (after! evil
  ;;   (add-hook! 'vterm-mode-hook
  ;;     (make-variable-buffer-local 'evil-normal-state-entry-hook)
  ;;     (make-variable-buffer-local 'evil-normal-state-exit-hook)
  ;;     (add-hook! 'evil-normal-state-entry-hook (vterm-copy-mode 1))
  ;;     (add-hook! 'evil-normal-state-exit-hook (vterm-copy-mode -1))
  ;;     ))
  ;; inserting inside a vterm should reset cursor position
  (map! :mode vterm-mode
        :n "o" (cmd!
                (vterm-reset-cursor-point)
                (evil-collection-vterm-insert)))
  ;; Override a bunch of keybindings during insert state in vterm to be more term friendly
  (defadvice! vterm-mappings () :after #'vterm-mode
    (map!
     (:mode vterm-mode
      :i "C-j" #'vterm-send-down
      :i "C-k" #'vterm-send-up)
     (:mode vterm-mode
      :map evil-insert-state-map
      "S-TAB" nil
      "<backtab>" nil)))
  ;; FIXME: ?
  (map! :map vterm-mode
        (:leader "C-c"
         :i "C-c" #'vterm-send-C-c))
  )

