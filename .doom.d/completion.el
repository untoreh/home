;;; ../../../var/home/fra/.doom.d/completion.el -*- lexical-binding: t; -*-

  (use-package! company-quickhelp
    :if (featurep! :completion company +tooltips)
    :init
    (setq-default company-quickhelp-mode t)
    :commands company-quickhelp-mode)


(use-package! company
  :if (featurep! :completion company)
  :init
  :config
  (use-package! company-files)
  (setq-default
   company-idle-delay 0.006
   company-echo-delay 0
   company-dabbrev-other-buffers t)
  ;; (set-company-backend!
  ;;   '(text-mode
  ;;     markdown-mode
  ;;     gfm-mode)
  ;;   'company-files
  ;;     )
  )

(map! "M-/" #'hippie-expand)

;; NOTE: tabnine uses north of 500M for its server
;; (use-package! company-tabnine
;;   :after company
;;   :config
;;   (cl-pushnew 'company-tabnine (default-value 'company-backends)))
