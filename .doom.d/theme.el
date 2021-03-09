;;; ../../../var/home/fra/.doom.d/theme.el -*- lexical-binding: nil; -*-

(setq doom-font (font-spec :family "Input Mono Compressed" :size 15 :weight 'normal)
	doom-variable-pitch-font (font-spec :family "Input Sans Condensed" :size 12))

(load! "ligatures")

(setq doom-theme 'doom-dracula
      doom-dracula-brighter-modeline t
      doom-dracula-colorful-headers t)

;; modeline
(use-package! nyan-mode
  :after doom-modeline
  :init
  (setq
   nyan-mode t
   nyan-animate-nyancat nil
   nyan-wavy-trail nil
   nyan-minimum-window-width 1024)
  :after-call
  doom-modeline-mode)
(use-package! parrot
  :after-call doom-modeline-mode
  :config
  (parrot-mode nil))

(setq-default
 window-combination-resize t
 x-stretch-cursor t)

(use-package! info-colors
  :after-call Info-mode
  :hook '(Info-selection-hook . info-colors-fontify-node))

(size-indication-mode t)
