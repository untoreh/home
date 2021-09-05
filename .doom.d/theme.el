;;; ../../../var/home/fra/.doom.d/theme.el -*- lexical-binding: nil; -*-

;; Input is SLOW with ligatures enabled!
;; (setq doom-font (font-spec :family "Input Mono Compressed" :size 15 :weight 'normal)
;; 	doom-variable-pitch-font (font-spec :family "Input Sans Condensed" :size 12))


(load! "missing-fonts")
(load! "ligatures")

(setq doom-theme 'doom-dracula
      doom-dracula-brighter-modeline t
      doom-dracula-colorful-headers t)

(setq
 doom-font (font-spec :family "Hack" :size 14 :weight 'normal)
 doom-big-font (font-spec :family "iA Writer Duospace" :size 24 :weight 'bold)
 doom-variable-pitch-font (font-spec :family "Barlow" :size 16)
 doom-unicode-font (font-spec :family "JuliaMono" :size 16)
 doom-serif-font (font-spec :family "Fantasque Sans Mono" :size 16))

(setq-default line-spacing 1)

;; nyan and parrot only in the "doom" modeline
(when (not (featurep! :ui modeline +light))
  (use-package! nyan-mode
    :if (boundp 'nyan-mode)
    :init
    (progn
      (setq
       nyan-animate-nyancat nil
       nyan-wavy-trail nil
       nyan-minimum-window-width 1024)
      (nyan-mode)))
  (use-package! parrot
    :if (boundp 'parrot-mode)
    :config
    (parrot-mode)))

(setq-default
 window-combination-resize t
 x-stretch-cursor t)

(use-package! info-colors
  :after-call Info-mode
  :hook '(Info-selection-hook . info-colors-fontify-node))

;; posframe
;(use-package! which-key-posframe
    ;:config
    ;(which-key-posframe-mode))
(use-package! hydra-posframe
  :config 
  (hydra-posframe-mode))

;; buffer size in the modeline
(size-indication-mode t)
;; use tooltips
(tooltip-mode t)
(setq
 ;; don't use gtk tooltips because of no face attrs
 x-gtk-use-system-tooltips nil
 )

(use-package! valign
  :init
  (if (boundp #'valign-remove-advice)
      (valign-remove-advice))
  :hook ((org-mode-hook markdown-mode-hook) . valign-mode))

(when (and (featurep! :ui treemacs)
           (featurep! :tools magit))
  (setq +treemacs-git-mode 'deferred))

;;
;; https://github.com/hlissner/doom-emacs/issues/2967
(after! doom-dracula-theme
  (custom-set-faces!
    '(mode-line :family "Input Mono Condensed" :height 0.99)
    '(mode-line-inactive :family "Input Mono Condensed" :height 0.99))
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
  )

;; miniframe
(use-package! mini-frame
  :config
  (setq
   mini-frame-detach-on-hide nil
   mini-frame-show-parameters
        '((bottom . 10)
          (width . 0.7)
          (left . 0.5)))
  (mini-frame-mode t))

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
