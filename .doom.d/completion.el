;;; ../../../var/home/fra/.doom.d/completion.el -*- lexical-binding: t; -*-

;; FIXME: WSL has focus problems, and tooltip is not shown unless the
;; mouse is moved a bit everytime completions are shown
(use-package! company-quickhelp
  :after company
  :if (or (featurep! :completion company +tooltips)
          (featurep! :completion company +childframe))
  :init
  (setq-default company-quickhelp-delay 0.33
                company-quickhelp-use-propertized-text t)
  :config
  (company-quickhelp-mode 1))

(add-hook 'doom-first-buffer-hook
          (defun +abbrev-file-name ()
            (setq-default abbrev-mode t)
            (setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))))


(defun +company/dabbrev-backend (command &optional arg &rest ignored)
  (if (derived-mode-p 'prog-mode)
      (company-dabbrev-code command arg ignored)
    (company-dabbrev command arg ignored)))

;; add dabbrev to languages backends
(set-company-backend! 'prog-mode
  '(:separate
    company-keywords
    company-yasnippet
    company-capf
    +company/dabbrev-backend
    ))

(map! :after company
      (:map company-active-map
       ;; NOTE: C-<tab> is used by `auto-yasnippet'
       "<tab>" #'company-complete-common
       "<backtab>" #'undo-tree-undo
       "C-s" #'company-search-candidates
       "C-f" #'company-filter-candidates
       ;; FIXME: upstream this
      "C-o" #'company-search-toggle-filtering)


      :i "<backtab>" #'back-to-indentation
      ;; this apparently is how to map `C-S-<tab>' (or `C-<backtab>')
      :i "C-<iso-lefttab>" #'tab-to-tab-stop)


;; TODO: lookup ggtags.el
(use-package! company
  :if (featurep! :completion company)
  :config
  (setq orderless-component-separator "[ &+-]")
  ;; make the common part work independently from the prefix
  ;; and without absolute matching (doesn't have to be shared among ALL candidates)
  (advice-add 'company-update-candidates :after
              #'my/company-update-first-common)
  ;; (advice-remove 'company-update-candidates
  ;;             #'my/company-update-first-common)
  (setq
   company-async-redisplay-delay 0.01
   company-idle-delay (if (featurep! :completion company +childframe) 0.33 0.1)
   company-tooltip-idle-delay 0.1

   company-selection-wrap-around t
   company-show-quick-access t
   company-tooltip-limit 10
   company-tooltip-flip-when-above t)


  ;; only use pseudo-tooltip frontend because we use
  ;; fancy-dabbrev-expand
  (setq company-frontends nil)
  (pushnew! company-frontends
            'company-pseudo-tooltip-frontend
            'company-preview-common-frontend
            ))


;; NOTE: not used because we decided to preview the _common_ part of candidates
;; we wouldn't use fancy-dabbrev
;; (use-package! fancy-dabbrev
;;   :after company
;;   :init
;;   ;; (Don't use hippie-expand) We complete context based stuff with company,
;;   ;; so only use general completion functions with hippie-expand
;;   ;; (after! hippie-exp
;;   ;;   ;; replace dabbrev-expand
;;   ;;   (setq hippie-expand-try-functions-list
;;   ;;         '(
;;   ;;           try-expand-dabbrev
;;   ;;           ;; `try-expand-dabbrev-visible' is most likely not worth it
;;   ;;           ;; so directly search all buffers, since they are ordered by
;;   ;;           ;; relevance anyway
;;   ;;           try-expand-dabbrev-all-buffers
;;   ;;           try-expand-list
;;   ;;           try-expand-list-all-buffers
;;   ;;           ;; there isn't a company backend for the kill-ring
;;   ;;           try-expand-dabbrev-from-kill
;;   ;;           ;; but inserting an entire paste with `try-expand-whole-kill'
;;   ;;           ;; might be...overkill
;;   ;;           ;;
;;   ;;           ;; line expansion is managed by `+company/whole-lines'
;;   ;;           ;; which is less noisy than `try-expand-line'
;;   ;;           ;;
;;   ;;           ;; `try-expand-all-abbrevs' would return too out-of-context candidates
;;   ;;           )))
;;   ;; (advice-add #'dabbrev-expand :override #'hippie-expand)
;;   :config
;;   (setq
;;    dabbrev-select-buffers-function #'+workspace-buffer-list
;;    ;; this are only used with the default dabbrev select buffers function
;;    ;; dabbrev-check-other-buffers t
;;    ;; dabbrev-check-all-buffers t
;;    ;; don't use fancy-dabbrev menus since we use company for it
;;    fancy-dabbrev-menu-height 0
;;    ;; make preview happen after company, otherwise the completion list has a bad offset
;;    fancy-dabbrev-preview-delay (string-to-number (format "%.2f" (+ company-idle-delay 0.01)))
;;    fancy-dabbrev-preview-context 'before-non-word
;;    fancy-dabbrev-expansion-context 'after-non-space)
;;   (global-fancy-dabbrev-mode 1))

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
  :if (featurep! :completion corfu)
  :defer
  :config
  (setq
   corfu-auto t
   corfu-auto-delay 0.005
   corfu-auto-prefix 1
   corfu-cycle t
   corfu-quit-at-boundary t)
  (corfu-global-mode))


