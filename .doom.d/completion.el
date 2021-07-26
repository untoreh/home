;;; ../../../var/home/fra/.doom.d/completion.el -*- lexical-binding: t; -*-


;; (use-package! company-quickhelp
;;   :after company
;;   :init
;;   (setq-default company-quickhelp-mode t)
;;   :commands company-quickhelp-mode
;;   :config
;;   )
(setq company-dabbrev-other-buffers t)

(use-package! company-files
  :after company)

(after! company
  ;; (if (featurep! :editor snippets)
  ;;     (setq-hook! company-mode-hook
  ;;       company-backends
  ;;       `(,(first company-backends)
  ;;         :with
  ;;         company-yasnippet
  ;;         company-files)))
  )
(map! "M-/" #'hippie-expand)

;; NOTE: tabnine uses north of 500M for its server
;; (use-package! company-tabnine
;;   :after company
;;   :config
;;   (cl-pushnew 'company-tabnine (default-value 'company-backends)))
