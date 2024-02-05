;;; completion/capes.el -*- lexical-binding: t; -*-

(use-package! cape
  :after corfu
  :init
  (defun my/capf-cmd! (capf)
    (cmd! (let ((completion-at-point-functions (list capf t)))
            (completion-at-point))))
  :config
  (setq cape-dict-file (my/concat-path doom-cache-dir "spell" "aspell-dict.txt" ))
  (setq dabbrev-upcase-means-case-search t)
  (setq case-fold-search nil)

  (setq
   cape-symbol-scapf (cape-capf-super #'cape-keyword #'cape-symbol #'cape-abbrev #'cape-dabbrev)
   cape-word-scapf (cape-capf-super #'cape-ispell #'cape-dict)
   cape-expand-scapf (cape-capf-super #'cape-line) ;; snippets/tempel
   cape-file-scapf (cape-capf-buster
                    (cape-capf-super #'cape-dabbrev
                                     #'cape-file
                                     #'cape-history))
   cape-char-scapf (cape-capf-super #'cape-tex #'cape-sgml #'cape-rfc1345)
   cape-default-scapf (cape-capf-buster #'cape-symbol-scapf)
   )
  (mapc (lambda (v) (defalias (intern (symbol-name v)) (symbol-value v)))
        '(cape-symbol-scapf cape-word-scapf cape-expand-scapf cape-file-scapf cape-char-scapf))


  (add-hook! nim-mode :depth -1
    (setq-local
     completion-at-point-functions
     '(cape-symbol-scapf cape-word-scapf)))
  (when (and (modulep! :tools lsp) (not (modulep! :tools lsp +eglot)))
    (setq-hook! 'lsp-completion-mode-hook
      completion-at-point-functions '(lsp-completion-at-point)))

  (setq-hook! markdown-mode
    completion-at-point-functions '(blog-tags-capf cape-word-scapf cape-expand-scapf cape-file-scapf))
  )

;; (when (modulep! :tools lsp +eglot)
;;   (setq cape-eglot-scapf
;;         (cape-capf-buster (cape-capf-super #'tabnine-capf #'eglot-completion-at-point)))
;;   (setq-hook! 'eglot-managed-mode-hook
;;     completion-at-point-functions (list cape-eglot-scapf t)))
