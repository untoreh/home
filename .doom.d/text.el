;;; ../../../var/home/fra/.doom.d/text.el -*- lexical-binding: t; -*-
;; #TODO: consider emacs-anywhere
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))
