;;; corfu.el -*- lexical-binding: t; -*-

(use-package! corfu
  ;; :custom
  ;; these should already be set by doom
  ;; (corfu-auto t)                
  ;; (corfu-cycle t)
  ;; (corfu-auto-prefix 2)
  ;; (corfu-echo-delay 0.3)
  ;; (corfu-quit-no-match 'separator)
  ;;
  ;; these are left to defaults for now
  ;; (corfu-preselect nil)
  ;; (corfu-on-exact-match 'quit)

  ;; no delay
  :init
  ;; these should already be set by doom
  ;; :hook (corfu-mode . corfu-popupinfo-mode)

  :config
  ;; (add-hook! emacs-elisp-mode (add-hook 'completion-at-point-functions #'codeium-completion-at-point -99 t))
  ;; (when (modulep! :lang julia)
  ;;   (add-hook! julia-mode (add-hook 'completion-at-point-functions #'codeium-completion-at-point -99 t)))
  (setq
   corfu-auto-delay 0.05
   corfu-auto-prefix 0
   corfu-preview-current 'insert
   )
  ;; (load! "capes")
  )

(use-package! corfu-quick
  :bind
  (:map corfu-map
        ("M-q" . corfu-quick-complete)
        ))
