;;; completion/copilot.el -*- lexical-binding: t; -*-

(use-package! copilot
  :if nil
  :after corfu
  :bind ("M-k" . copilot-accept-completion-by-word) ;;  TODO: which :map should be used?
  :ensure t
  :hook ((prog-mode text-mode) . copilot-mode)
  :bind
  (("C-c n" . copilot-next-completion)
   ("C-c p" . copilot-previous-completion))
  :config
  (set-face-foreground 'copilot-overlay-face "pink")

  (defun +my/corfu-candidates-p ()
    (or (not (eq corfu--candidates nil))
        (derived-mode-p 'minibuffer-mode)
        (not (looking-back "[\x00-\xff]"))))

  (customize-set-variable 'copilot-enable-predicates '(evil-insert-state-p))
  (customize-set-variable 'copilot-disable-predicates '(+my/corfu-candidates-p))
  )
