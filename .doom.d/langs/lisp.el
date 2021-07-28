;; -*- lexical-binding: t; -*-
;;
(defun my/elisp-prompt-regexp () (setq defun-prompt-regexp "(defun"))
(add-hook! 'emacs-lisp-mode-hook #'my/elisp-prompt-regexp)
