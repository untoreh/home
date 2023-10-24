;;; ../../../var/home/fra/.doom.d/org-misc.el -*- lexical-binding: t; -*-

;; needed by poly-org-mode to be nil in all buffers
(setq org-src-fontify-natively (if (modulep! :lang org +poly) nil t))
;; why would I need to indent src blocks?
(setq org-edit-src-content-indentation 0)
(setq org-fontify-quote-and-verse-blocks t)

;; fail org mode hooks functions gracefully
(defadvice! shut-up-org-problematic-hooks (orig-fn &rest args)
  :around #'org-fancy-priorities-mode
  (ignore-errors (apply orig-fn args)))

;; TODO: try this in other places that feel slow
(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))
(add-hook 'org-mode-hook #'locally-defer-font-lock)

;; don't format org mode buffers on save
(if (modulep! :editor format +onsave)
    (add-to-list '+format-on-save-enabled-modes 'org-mode t))
