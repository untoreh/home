;;; ../../../var/home/fra/.doom.d/langs/langs.el -*- lexical-binding: t; -*-

(setq enabled-langs '(python rust julia racket raku json markdown org emacs-lisp))

(use-package! tree-sitter
  :after doom-themes
  :commands (global-tree-sitter-mode)
  :config
  (require 'tree-sitter-langs)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(if (featurep! :lang shell)
    (load! "shell"))
(if (featurep! :lang python)
    (load! "python"))
(if (featurep! :lang julia)
   (load! "julia"))
(if (featurep! :tools lsp)
    (load! "lsp"))
