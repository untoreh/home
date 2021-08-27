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
  (setq
   company-async-redisplay-delay 0.005
   company-echo-delay 0.006
   company-idle-delay 0
   company-dabbrev-other-buffers t
   company-selection-wrap-around t)
  (map! :i "C-SPC" #'company-complete-common-or-cycle)
  (map! :i "C-/" #'hippie-expand)
  ;; instant completion with no candidates
  (pushnew! company-frontends 'company-preview-if-just-one-frontend)
  ;;
  (delq! 'company-echo-metadata-frontend company-frontends))

;; NOTE: tabnine uses north of 500M for its server
;; (use-package! company-tabnine
;;   :after company
;;   :config
;;   (cl-pushnew 'company-tabnine (default-value 'company-backends)))

;; Doesn't seem to work OOB
;; (use-package! company-fuzzy
;;   :config
;;   (global-company-fuzzy-mode 1))

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

(setq tab-always-indent t
      completion-cycle-threshold 3)
(use-package! corfu
  :when (featurep! :completion corfu)
  :config
  (setq
   corfu-auto t
   corfu-auto-delay 0.005
   corfu-auto-prefix 1
   corfu-cycle t
   corfu-quit-at-boundary t)
  (corfu-global-mode))

