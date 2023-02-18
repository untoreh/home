;;; .archive/langs/julia.el -*- lexical-binding: t; -*-

;; replaced by lsp formatter
(use-package! julia-formatter
  :if (modulep! :lang julia +format)
  :commands julia-mode
  :hook (julia-mode julia-formatter--ensure-server)
  :config
  (defun julia-formatter-format-string (str)
    " Formatting function to pass to `format-all--buffer-thunk' "
    (julia-formatter--ensure-server)
    (condition-case nil
        (let* ((response (jsonrpc-request
                          julia-formatter--server-process-connection
                          :format
                          (list :text
                                (save-match-data
                                  (thread-first str
                                                (split-string  "\n" nil)
                                                (vconcat)))
                                :current_line 1))))
          (insert (mapconcat 'identity response "\n"))
          ;; success
          '(nil nil))
      ;; failure
      '(t response)))
  (set-formatter!
    'jl-julia-formatter
    #'julia-formatter-format-string
    :modes '(julia-mode))
  ;; override lsp formatting
  (setq-hook! 'julia-mode-hook +format-with-lsp nil))

;; replaced by corfu
;; (set-company-backend! 'julia-mode
;;   '(:separate company-capf company-yasnippet company-dabbrev-code company-files))
