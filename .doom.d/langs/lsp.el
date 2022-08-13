;;; ../../../var/home/fra/.doom.d/langs/lsp.el -*- lexical-binding: t; -*-

(after! (lsp-mode rx)
  (setq lsp-enable-file-watchers nil)
  (pushnew! lsp-file-watch-ignored-directories (rx "\\/home\\/fra")))

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
  (put 'lsp-restart 'safe-local-variable (lambda (&rest args) t))
  (setq
   ;; for performance
   lsp-use-plists t
   lsp-restart 'ignore
   lsp-auto-configure t
   ;; NOTE: Can't guess root since $HOME is considered a workspace...
   lsp-auto-guess-root nil
   lsp-eldoc-enable-hover t
   lsp-signature-render-documentation nil
   lsp-lens-enable t
   lsp-headerline-breadcrumb-enable t
   lsp-modeline-diagnostics-enable nil
   lsp-modeline-code-actions-enable nil
   lsp-semantic-tokens-enable t
   lsp-enable-folding t
   lsp-enable-imenu t
   lsp-enable-snippet t
   )
  (after! lsp-ui
    (setq
     lsp-ui-imenu-enable t
     ;; doc frames with mouse hover
     lsp-ui-doc-use-childframe t
     ;; webkit causes crashes
     lsp-ui-doc-use-webkit nil
     lsp-ui-doc-enable t
     lsp-ui-doc-max-height 80
     lsp-ui-doc-max-width 80
     lsp-ui-doc-show-with-mouse t
     lsp-ui-doc-show-with-cursor nil
     ;;
     lsp-ui-sideline-enable nil
     lsp-ui-sideline-show-code-actions t
     lsp-ui-sideline-show-hover t
     lsp-ui-sideline-show-diagnostics t))
  (after! lsp-completion
    (setq
     lsp-completion-show-detail t
     lsp-completion-show-kind t)
    ;; lsp-diagnostics-provider
    ;; lsp-completion-provider
    ;; text
    )
  (when (featurep! :ui treemacs +lsp)
    (after! treemacs
      (lsp-treemacs-sync-mode 1)))
  )
