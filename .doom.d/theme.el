;;; ../../../var/home/fra/.doom.d/theme.el -*- lexical-binding: nil; -*-

;; Input is SLOW with ligatures enabled!
;; (setq doom-font (font-spec :family "Input Mono Compressed" :size 15 :weight 'normal)
;; 	doom-variable-pitch-font (font-spec :family "Input Sans Condensed" :size 12))
(setq
 doom-font (font-spec :family "Hack" :size 14 :weight 'normal)
 doom-big-font (font-spec :family "iA Writer Duospace" :size 24 :weight 'bold)
 doom-variable-pitch-font (font-spec :family "Barlow" :size 16)
 doom-serif-font (font-spec :family "Fantasque Sans Mono" :size 16)
 doom-unicode-font (font-spec :family "JuliaMono"))

(setq-default line-spacing 1)

(load! "ligatures")

(setq doom-theme 'doom-dracula
      doom-dracula-brighter-modeline t
      doom-dracula-colorful-headers t)

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
(after! doom-modeline
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
