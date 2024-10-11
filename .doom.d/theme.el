;;; ../../../var/home/fra/.doom.d/theme.el -*- lexical-binding: nil; -*-

;; Input is SLOW with ligatures enabled!
;; (setq doom-font (font-spec :family "Input Mono Compressed" :size 15 :weight 'normal)
;; 	doom-variable-pitch-font (font-spec :family "Input Sans Condensed" :size 12))


(load! "missing-fonts")

(setq doom-theme 'doom-dracula
      doom-dracula-brighter-modeline t
      doom-dracula-colorful-headers t)

(setq-default
 doom-font (font-spec :family "Hack" :size 14 :weight 'regular)
 doom-big-font (font-spec :family "iA Writer Duo S" :size 24 :weight 'bold)
 doom-variable-pitch-font (font-spec :family "Barlow" :size 16)
 doom-unicode-font (font-spec :family "JuliaMono")
 doom-serif-font (font-spec :family "Fantasque Sans Mono" :size 14)
 )

(setq-default line-spacing 1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq-default
 window-combination-resize t
 x-stretch-cursor t)

;; posframe
                                        ;(use-package! hydra-posframe
                                        ;:if (display-graphic-p)
                                        ;:config
                                        ;(setq hydra-posframe-poshandler #'posframe-poshandler-frame-bottom-center)
                                        ;(hydra-posframe-mode))

;; buffer size in the modeline
(size-indication-mode t)
;; use tooltips
(tooltip-mode t)
;; (setq
;;  ;; don't use gtk tooltips because of no face attrs
;;  x-gtk-use-system-tooltips nil
;;  )

;;
;; https://github.com/hlissner/doom-emacs/issues/2967
;; (after! doom-dracula-theme
;;   (custom-set-faces!
;;     '(mode-line :family "Input Mono Condensed" :height 0.99)
;;     '(mode-line-inactive :family "Input Mono Condensed" :height 0.99))
;; (add-hook! 'doom-modeline-mode-hook
;;   (let ((char-table char-width-table))
;;     (while (setq char-table (char-table-parent char-table)))
;;     (dolist (pair doom-modeline-rhs-icons-alist)
;;       (let ((width 2)  ; <-- tweak this
;;             (chars (cdr pair))
;;             (table (make-char-table nil)))
;;         (dolist (char chars)
;;           (set-char-table-range table char width))
;;         (optimize-char-table table)
;;         (set-char-table-parent table char-table)
;;         (setq char-width-table table))))
;;   )
;; (doom-modeline-def-modeline 'main
;;     '(bar matches buffer-info remote-host buffer-position parrot selection-info)
;;     '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs "  "))
;; )

;; candidates window
(use-package! vertico-posframe
  :if (and (display-graphic-p) (modulep! :completion vertico +childframe))
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  (after! vertico
    (setq vertico-multiform-commands
          '((consult-line
             posframe
             (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
             (vertico-posframe-border-width . 10)
             ;; NOTE: This is useful when emacs is used in both in X and
             ;; terminal, for posframe do not work well in terminal, so
             ;; vertico-buffer-mode will be used as fallback at the
             ;; moment.
             (vertico-posframe-fallback-mode . vertico-buffer-mode))
            (t posframe)))
    (vertico-multiform-mode 1)
    ))

;; frame title
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p (file-truename org-directory) (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "≋ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ● %s" " ○ %s") project-name))))))

;; doc colors
(use-package! info-colors
  :commands (info-colors-fontify-node))
(add-hook 'Info-selection-hook 'info-colors-fontify-node)

;; NOTE: theme magic is not useful if on windows, also not really a fun of "absolute" customization,
;; every app should choose the colorscheme that it finds works best with their ui, trying to make
;; everything "fit together" is 1. a lost cause, 2. counter-productive. What is important is to make themes
;; "play" nice with each other, adjusting gammas and transparencies. If everything uses just once theme it gets dull.

(use-package! keycast
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" mode-line-keycast " ") global-mode-string))))
  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
      :height 0.9)
    '(keycast-key :inherit custom-modified
      :height 1.1
      :weight bold)))

;; NOTE: gif-screencast needs support for WSL, as a windows-side binary
;; has to be used for screen recording
(use-package! gif-screencast
  :commands gif-screencast-mode
  :config
  (map! :mode gif-screencast-mode
        :desc "start recording"
        "<f9>" #'gif-screencast-start-or-stop))

(use-package! emojify
  :if (modulep! :ui emoji)
  :defer
  :init
  ;; don't emojify strings
  (setq emojify-program-contexts '(comments)
	;; don't emojify ascii
	emojify-emoji-styles '(github unicode))
  :config
  ;; disable emojify mode on vterm buffers of languages
  (after! (vterm functions)
    (pushnew! emojify-inhibit-in-buffer-functions #'my/repl-vterm-bufferp)
    (pushnew! emojify-inhibit-major-modes #'vterm-mode)))

(use-package! indent-bars
  :hook ((prog-mode text-mode conf-mode) . indent-bars-mode)
  :config
  (require 'indent-bars-ts)
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.15)
   indent-bars-pattern "."
   indent-bars-width-frac 0.1
   indent-bars-pad-frac 0.1
   indent-bars-zigzag nil
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
   indent-bars-highlight-current-depth '(:blend 0.5) ; pump up the BG blend on current
   indent-bars-display-on-blank-lines t)
  ;; just in case
  (add-hook! 'org-mode-local-vars-hook
    (defun +indent-guides-disable-maybe-h ()
      (and (bound-and-true-p highlight-indent-guides-mode)
           (org-indent-mode)
           (indent-bars-mode -1))))
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                if_statement with_statement while_statement)))
  )
