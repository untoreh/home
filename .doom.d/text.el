;;; ../../../var/home/fra/.doom.d/text.el -*- lexical-binding: t; -*-
;; TODO: consider emacs-anywhere

;; maybe only use indent guides for prog-mode?
;; never enable indent guides by default
;; (remove-hook! (prog-mode text-mode conf-mode) highlight-indent-guides-mode)
(use-package! highlight-indent-guides
  :if (modulep! :ui indent-guides)
  :defer
  :config
  ;; bitmap seems smoother than character
  (setq
   highlight-indent-guides-method 'character
   highlight-indent-guides-responsive 'top))

(when (modulep! :editor word-wrap)
  (add-transient-hook! 'text-mode-hook
    (+global-word-wrap-mode +1)))

;; evil
(after! evil
  (setq
   evil-ex-substitute-global t
   evil-kill-on-visual-paste nil))

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
(add-hook! 'doom-first-buffer-hook 'electric-pair-mode 'show-paren-mode)

;; calibre
(use-package! calibredb
  :init
  (map! :desc "calibredb" :leader "o l" #'calibredb)
  :commands (calibredb)
  :config
  (load! "calibredb-config.el"))
