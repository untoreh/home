;;; ../../../var/home/fra/.doom.d/langs/lsp.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (setq lsp-enable-file-watchers nil))

;; dap
(defun dap/debug-re ()
  (interactive)
  (dap-delete-all-sessions)
  (kill-matching-buffers "Launch Debug" nil t)
  (dap-debug-last))

(setq-default
 lsp-auto-configure t
 lsp-auto-guess-root t
 lsp-ui-doc-use-childframe t
 lsp-ui-doc-use-webkit nil)
