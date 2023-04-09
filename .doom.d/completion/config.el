;;; completion/config.el -*- lexical-binding: t; -*-

(setq-default orderless-component-separator "[ &+-]")
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)
(setq tab-always-indent 'complete
      completion-cycle-threshold 3)
(add-to-list 'corfu-auto-commands 'grammatical-edit-open-round)
(add-to-list 'corfu-auto-commands 'grammatical-edit-open-bracket)
(add-to-list 'corfu-auto-commands 'grammatical-edit-open-curly)

(advice-add #'keyboard-quit :before #'corfu-quit)
(add-to-list 'corfu-auto-commands 'end-of-visual-line)

;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
(advice-add 'corfu--setup :after 'evil-normalize-keymaps)
(advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
(evil-make-overriding-map corfu-map)

(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input))
    ;; (setq-local corfu-auto nil) Enable/disable auto completion
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
