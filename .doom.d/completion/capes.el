;;; completion/capes.el -*- lexical-binding: t; -*-

(use-package! cape
  :after (corfu)
  :config
  (setq cape-dict-file (my/concat-path doom-cache-dir "spell" "aspell-dict.txt" ))
  (setq dabbrev-upcase-means-case-search t)
  (setq case-fold-search nil))

(setq
 cape-symbol-scapf (cape-super-capf #'cape-keyword #'cape-symbol #'cape-abbrev #'cape-dabbrev)
 cape-word-scapf (cape-super-capf #'cape-ispell #'cape-dict)
 cape-expand-scapf (cape-super-capf #'cape-line) ;; snippets/tempel
 cape-file-scapf (cape-capf-buster
                  (cape-super-capf #'cape-dabbrev
                                   #'cape-file
                                   #'cape-history))
 cape-char-scapf (cape-super-capf #'cape-tex #'cape-sgml #'cape-rfc1345)
 cape-default-scapf (cape-capf-buster #'tabnine-capf)
 cape-t9-lsp-scapf (cape-super-capf #'lsp-completion-at-point #'tabnine-capf)
 )
(mapc (lambda (v) (defalias (intern (symbol-name v)) (symbol-value v)))
      '(cape-symbol-scapf cape-word-scapf cape-expand-scapf cape-file-scapf cape-char-scapf cape-t9-lsp-scapf))

(defun my/capf-cmd! (capf)
  (cmd! (let ((completion-at-point-functions (list capf t)))
          (completion-at-point))))

;; (when (modulep! :tools lsp +eglot)
;;   (setq cape-eglot-scapf
;;         (cape-capf-buster (cape-super-capf #'tabnine-capf #'eglot-completion-at-point)))
;;   (setq-hook! 'eglot-managed-mode-hook
;;     completion-at-point-functions (list cape-eglot-scapf t)))

(add-hook! nim-mode :depth -1
  (setq-local
   completion-at-point-functions
   '(cape-symbol-scapf cape-t9-lsp-scapf)))
(when (and (modulep! :tools lsp) (not (modulep! :tools lsp +eglot)))
  (setq-hook! 'lsp-completion-mode-hook
    completion-at-point-functions '(cape-t9-lsp-scapf)))

(setq-hook! markdown-mode
  completion-at-point-functions '(blog-tags-capf cape-word-scapf cape-expand-scapf cape-file-scapf))
