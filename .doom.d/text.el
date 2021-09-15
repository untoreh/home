;;; ../../../var/home/fra/.doom.d/text.el -*- lexical-binding: t; -*-
;; TODO: consider emacs-anywhere
;;
;; enable vimish-fold for persistent folds
(when (featurep! :editor fold)
  (use-package! vimish-fold
    :config
    (vimish-fold-global-mode 1)))

;; maybe only use indent guides for prog-mode?
;; never enable indent guides by default
;; (remove-hook! (prog-mode text-mode conf-mode) highlight-indent-guides-mode)
;; (when (featurep! :ui indent-guides)
;;   (add-hook 'prog-mode-hook
;;             (lambda () (highlight-indent-guides-mode 1))))

(when (featurep! :editor word-wrap)
  (add-transient-hook! 'text-mode-hook
    (lambda () (+global-word-wrap-mode 1)))
  )

;; evil
(after! evil
  (setq evil-kill-on-visual-paste nil))

;; (use-package! vlf-setup
;;   :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;; smartparens config
;; (after! smartparens
;;   (global-subword-mode 1)
;;   (show-smartparens-global-mode 1)
;;   (setq-default sp-use-subword t
;;                 sp-highlight-pair-overlay t
;;                 sp-highlight-wrap-overlay t
;;                 sp-highlight-wrap-tag-overlay t))

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
(add-hook! 'doom-first-buffer-hook 'electric-pair-mode 'show-paren-mode)

;; string inflection
;; https://github.com/akicho8/string-inflection
;;

(load! "calibredb-config.el")
