;; for terminal emulator
(setq-default multi-term-program "/usr/bin/zsh")
(evil-set-initial-state 'term-mode 'emacs)
(with-eval-after-load 'evil-escape
  (push 'term-mode evil-escape-excluded-major-modes))
(evil-define-key 'emacs term-raw-map (kbd "C-c") 'term-send-raw)
(evil-define-key 'emacs term-raw-map (kbd "ESC") 'term-send-raw)
(evil-define-key 'emacs term-raw-map (kbd "C-r") 'term-send-raw)
(evil-define-key 'emacs term-raw-map (kbd "<f1>") "\^[OP")
(evil-define-key 'emacs term-raw-map (kbd "<f2>") "\^[OQ")
(evil-define-key 'emacs term-raw-map (kbd "<f3>") "\^[OR")
(evil-define-key 'emacs term-raw-map (kbd "<f4>") "\^[OS")
(evil-define-key 'normal evil-normal-state-map (kbd "<f4>") "\^[OS")
