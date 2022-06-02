;;; ../../../var/home/fra/.doom.d/completion.el -*- lexical-binding: t; -*-

(load! "corfu")

(use-package! xref
  :config
  (setq xref-search-program 'ripgrep)
  (add-hook! xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package! devdocs
  :commands (devdocs-lookup-at-point devdocs-search-at-point)
  :config
  (setq-hook! python-mode
    devdocs-current-docs '("python~3.9" "PyTorch" "NumPy~1.20"))
  (setq-hook! emacs-lisp-mode
    devdocs-current-docs '("elisp"))
  (defun devdocs-lookup-at-point()
    (interactive)
    (devdocs-lookup nil (thing-at-point 'symbol)))
  (defun devdocs-search-at-point()
    (interactive)
    (devdocs-search (thing-at-point 'symbol t)))
  (map! :leader
        "s k" #'devdocs-lookup-at-point
        "s K" #'devdocs-search-at-point))

;; use a single abbrev file for multiple modes
(add-hook! 'doom-first-buffer-hook
          (defun +abbrev-file-name ()
            (setq-default abbrev-mode t)
            (setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))))

;; (map! :after corfu
;;       :i "<backtab>" #'back-to-indentation
;;       ;; this apparently is how to map `C-S-<tab>' (or `C-<backtab>')
;;       :i "C-<iso-lefttab>" #'tab-to-tab-stop)
