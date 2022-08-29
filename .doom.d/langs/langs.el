;;; ../../../var/home/fra/.doom.d/langs/langs.el -*- lexical-binding: t; -*-

(setq enabled-langs '(python rust julia racket raku json markdown org emacs-lisp))


(load! "tree-sitter")
(load! "lisp")
(load! "spell")
(if (featurep! :lang shell)
    (load! "shell"))
(if (featurep! :lang python)
    (load! "python"))
(if (featurep! :lang julia)
    (load! "julia"))
;; FIXME: nim module is disabled because useless
(if (or t (featurep! :lang nim))
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

;; ;; dont format snippets (list is negated)
;; ;; FIXME: this produces a eager macro expansion error on startup
(when (featurep! :editor format +onsave)
  (cl-dolist (mode '(snippet-mode
                     web-mode
                     org-msg-edit-mode
                     gitignore-mode
                     lisp-data-mode
                     conf-space-mode
                     gfm-mode
                     shell-mode
                     python-mode))
    (pushnew! +format-on-save-enabled-modes mode)))

;; flycheck
(after! flycheck-posframe
  (setq flycheck-posframe-border-width 5)
  (flycheck-posframe-configure-pretty-defaults)
  )

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


;; ;; make compilation buffers follow
(add-hook! (compilation-mode nim-compile-mode)
  (set (make-local-variable 'window-point-insertion-type) t))
