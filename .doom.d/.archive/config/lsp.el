;; lsp
(after! lsp-mode
  (setq lsp-enable-file-watchers nil))

;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; (add-hook 'php-mode-hook #'lsp-deferred)
;; (add-hook 'go-mode-hook #'lsp-deferred)
;; (setq-default lsp-auto-configure t
;;               lsp-auto-guess-root t
;;               lsp-ui-flycheck-enable t
;;               company-lsp-async t
;;               company-lsp-enable-snippet t
;;               company-lsp-enable-recompletion nil
;;               lsp-ui-doc-use-childframe t
;;               lsp-ui-doc-use-webkit nil
;;               )

;; (eval-after-load 'company (lambda()
;;                             (push 'company-lsp company-backends)))
;; (eval-after-load 'lsp-mode (lambda()
;;                              (lsp-register-client
;;                               (make-lsp-client :new-connection (lsp-stdio-connection "phpls")
;;                                                :major-modes '(web-mode)
;;                                                :server-id 'php))
;;                              (lsp-register-client
;;                               (make-lsp-client :new-connection (lsp-stdio-connection "phpls")
;;                                                :major-modes '(php-mode)
;;                                                :server-id 'php))

;;                              (lsp-register-client
;;                               (make-lsp-client :new-connection (lsp-stdio-connection "gols")
;;                                                :major-modes '(go-mode)
;;                                                :server-id 'go))
;;                              ))
;; (dolist (hook '(php-mode-hook web-mode-hook shell-script-mode-hook))
;;   (add-hook hook #'lsp-deferred))

;; dap
(defun dap/debug-re ()
  (interactive)
  (dap-delete-all-sessions)
  (kill-matching-buffers "Launch Debug" nil t)
  (dap-debug-last)
  )
