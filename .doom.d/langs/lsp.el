;;; ../../../var/home/fra/.doom.d/langs/lsp.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (setq lsp-enable-file-watchers nil))

;; dap
(defun dap/debug-re ()
  (interactive)
  (dap-delete-all-sessions)
  (kill-matching-buffers "Launch Debug" nil t)
  (dap-debug-last))

(use-package! lsp-mode
  :if (and (featurep! :tools lsp)
	   (not (featurep! :tools lsp +eglot)))
  :config
  (setq
   lsp-auto-configure t
   lsp-auto-guess-root t
   ;; doc frames with mouse hover
   lsp-ui-doc-use-childframe t
   lsp-ui-doc-use-webkit t
   lsp-ui-doc-enable t
   lsp-ui-doc-max-height 80
   lsp-ui-doc-max-width 80
   lsp-ui-doc-show-with-mouse t
   lsp-ui-doc-show-with-cursor nil
   ;;
   lsp-lens-enable t
   lsp-headerline-breadcrumb-enable t
   lsp-ui-sideline-enable t
   lsp-ui-sideline-show-code-actions t
   lsp-ui-sideline-show-hover t
   lsp-modeline-code-actions-enable t
   lsp-ui-sideline-show-diagnostics t
   lsp-eldoc-enable-hover t
   lsp-modeline-diagnostics-enable t
   lsp-signature-render-documentation t
   lsp-completion-show-detail t
   lsp-completion-show-kind t
   ;; lsp-diagnostics-provider
   ;; lsp-completion-provider
   ;; text
   )
  (when (featurep! :ui treemacs +lsp)
	  (lsp-treemacs-sync-mode 1))
  )
