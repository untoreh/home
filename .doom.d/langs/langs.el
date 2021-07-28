;;; ../../../var/home/fra/.doom.d/langs/langs.el -*- lexical-binding: t; -*-

(setq enabled-langs '(python rust julia racket raku json markdown org emacs-lisp))

(use-package! tree-sitter
  :when (bound-and-true-p module-file-suffix)
  :hook (prog-mode . tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-langs)
  (defadvice! doom-tree-sitter-fail-gracefully-a (orig-fn &rest args)
    "Don't break with errors when current major mode lacks tree-sitter support."
    :around #'tree-sitter-mode
    (condition-case e
        (apply orig-fn args)
      (error
       (unless (string-match-p (concat "^Cannot find shared library\\|"
                                       "^No language registered\\|"
                                       "cannot open shared object file")
                            (error-message-string e))
            (signal (car e) (cadr e)))))))

(load! "lisp")
(if (featurep! :lang shell)
    (load! "shell"))
(if (featurep! :lang python)
    (load! "python"))
(if (featurep! :lang julia)
   (load! "julia"))
(if (featurep! :tools lsp)
    (load! "lsp"))

;; prefer gfm-mode over markdown-mode
(let ((md-key (car (rassq 'markdown-mode auto-mode-alist))))
  (setq auto-mode-alist (rassq-delete-all 'markdown-mode auto-mode-alist))
  (pushnew! auto-mode-alist `(,md-key . gfm-mode)))


;; dont format snippets (list is negated)
(add-to-list '+format-on-save-enabled-modes 'snippet-mode 'append)
(add-to-list '+format-on-save-enabled-modes 'web-mode 'append)
(add-to-list '+format-on-save-enabled-modes 'org-msg-edit-mode 'append)
(add-to-list '+format-on-save-enabled-modes 'gitignore-mode 'append)
(add-to-list '+format-on-save-enabled-modes 'lisp-data-mode 'append)
(add-to-list '+format-on-save-enabled-modes 'conf-space-mod 'append)
(add-to-list '+format-on-save-enabled-modes 'gfm-mode 'append)
(add-to-list '+format-on-save-enabled-modes 'shell-mode 'append)
(add-to-list '+format-on-save-enabled-modes 'python-mode 'append)
