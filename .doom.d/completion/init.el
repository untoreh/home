;;; corfu.el -*- lexical-binding: t; -*-

(use-package! corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 0)
  (corfu-auto-delay 0.3)
  (corfu-echo-documentation 0.3)
  (corfu-quit-no-match 'separator)        ;; Automatically quit if there is no match
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match 'quit)
  :init
  (global-corfu-mode)
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (load! "config")
  (load! "icons")
  (load! "capes")
  (load! "bindings")
  ;; FIXME: basic completion style causes infinite recursion
  ;; (setq-hook! 'evil-insert-state-entry-hook
  ;;   completion-styles '(basic))
  ;; (setq-hook! 'evil-insert-state-exit-hook
  ;;   completion-styles '(orderless))
  )
(use-package! corfu-quick
  :bind
  (:map corfu-map
        ("C-q" . corfu-quick-insert)))
