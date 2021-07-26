;;; ../../../var/home/fra/.doom.d/completion.el -*- lexical-binding: t; -*-

(when (featurep! :completion company)
  (use-package! company-quickhelp
    :init
    (setq-default company-quickhelp-mode t)
    :commands company-quickhelp-mode)
  (use-package! company-files))

(use-package! company
  :init
  :config
  (setq-default
   company-idle-delay 0.006
   company-echo-delay 0
   company-dabbrev-other-buffers t)
  (set-company-backend!
    '(text-mode
      markdown-mode
      gfm-mode)
    '(:seperate
      company-files
      company-yasnippet)))

(map! "M-/" #'hippie-expand)

;; NOTE: tabnine uses north of 500M for its server
;; (use-package! company-tabnine
;;   :after company
;;   :config
;;   (cl-pushnew 'company-tabnine (default-value 'company-backends)))
