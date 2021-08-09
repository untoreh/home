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
    (lambda () (+global-word-wrap-mode 1))))
