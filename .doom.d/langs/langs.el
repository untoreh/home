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
(load! "spell")
(if (featurep! :lang shell)
    (load! "shell"))
(if (featurep! :lang python)
    (load! "python"))
(if (featurep! :lang julia)
   (load! "julia"))
(if (featurep! :lang nim)
    (load! "nim"))
(if (featurep! :tools lsp)
    (load! "lsp"))

;; prefer gfm-mode over markdown-mode
(let ((md-key (car (rassq 'markdown-mode auto-mode-alist))))
  (setq auto-mode-alist (rassq-delete-all 'markdown-mode auto-mode-alist))
  (pushnew! auto-mode-alist `(,md-key . gfm-mode)))


(setq markdown-header-scaling t
      markdown-header-scaling-values
      '(1.25 1.15 1.08 1.00 0.90 0.75))

;; dont format snippets (list is negated)
;; FIXME: this produces a eager macro expansion error on startup
(when (featurep! :editor format +onsave)
  (dolist ((mode '(snippet-mode
                   web-mode
                   org-msg-edit-mode
                   gitignore-mode
                   lisp-data-mode
                   conf-space-mode
                   gfm-mode
                   shell-mode
                   python-mode)))
    (add-to-list '+format-on-save-enabled-modes mode 'append)))


(defun my/repl-vterm-bufferp (&rest args)
  "Check if the current buffer is a repl vterm buffer of any language in `enabled-langs'."
  (catch 'enabled
    (mapc (lambda (l)
            (let ((mode (intern (concat
                                 (symbol-name l)
                                 "-repl-vterm-mode"))))
              (when (and (boundp mode)
                         (symbol-value mode))
                (throw 'enabled t))))
          enabled-langs)
    nil))

;; disable emojify mode on vterm buffers of languages
(pushnew! emojify-inhibit-in-buffer-functions 'my/repl-vterm-bufferp)

;; make compilation buffers follow
(add-hook! compilation-mode
 (set (make-local-variable 'window-point-insertion-type) t))
