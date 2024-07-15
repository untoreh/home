;;; corfu.el -*- lexical-binding: t; -*-

(use-package! corfu
  :custom
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
  (corfu-auto-delay 0.0)
  :init
  ;; these should already be set by doom
  ;; :hook (corfu-mode . corfu-popupinfo-mode)

  :config
  ;; (load! "capes")
  )

(use-package! corfu-quick
  :bind
  (:map corfu-map
        ("M-q" . corfu-quick-complete)
        ))
