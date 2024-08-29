;;; ../../../var/home/fra/.doom.d/langs/langs.el -*- lexical-binding: t; -*-

(load! "lisp")
(add-transient-hook! 'text-mode
  (load! "spell"))
(load! "aiml")
(if (modulep! :lang shell)
    (load! "shell"))
(if (modulep! :lang python)
    (load! "python"))
(if (modulep! :lang julia)
    (load! "julia"))
;; FIXME: nim module is disabled because useless
(if (or t (modulep! :lang nim))
    (load! "nim"))
(if (modulep! :tools lsp)
    (load! "lsp"))

;; prefer gfm-mode over markdown-mode
(let ((md-key (car (rassq 'markdown-mode auto-mode-alist))))
  (setq auto-mode-alist (rassq-delete-all 'markdown-mode auto-mode-alist))
  (pushnew! auto-mode-alist `(,md-key . gfm-mode)))


(setq markdown-header-scaling t
      markdown-header-scaling-values
      '(1.25 1.15 1.08 1.00 0.90 0.75))

;; ;; dont format snippets (list is negated)
;; ;; FIXME: this produces a eager macro expansion error on startup
(when (modulep! :editor format +onsave)
  (cl-dolist (mode '(snippet-mode
                     web-mode
                     org-msg-edit-mode
                     gitignore-mode
                     lisp-data-mode
                     conf-space-mode
                     gfm-mode
                     shell-mode
                     python-mode))
    (pushnew! +format-on-save-disabled-modes mode)))

;; ;; make compilation buffers follow
(add-hook! (compilation-mode nim-compile-mode)
  (set (make-local-variable 'window-point-insertion-type) t))

(use-package! aas
  :config
  (when (modulep! :lang julia)
    (add-hook! julia-mode #'aas-activate-for-major-mode)
    (aas-set-snippets 'julia-mode
      ;; expand unconditionally
      "^@d" "@doc \"\"\"\n\"\"\""
      )))

(when (modulep! :tools tree-sitter)
  (setq treesit-extra-load-path (list (my/concat-path tree-sitter-langs-grammar-dir "bin")))
  )
