;;; ../../../var/home/fra/.doom.d/text.el -*- lexical-binding: t; -*-
;; TODO: consider emacs-anywhere
;; use gfm-mode for markdown by default
;; (after! markdown-mode
;;   (setcdr
;;    (assoc
;;     "\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'"
;;     auto-mode-alist)
;;    'gfm-mode))
;; (setq auto-mode-alist
;;       (delete '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)
;;               auto-mode-alist))

;; enable vimish-fold for persistent folds
(when (featurep! :editor fold)
  (after! vimish-fold
    (vimish-fold-global-mode 1)))
