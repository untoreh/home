;;; ../../../var/home/fra/.doom.d/org.el -*- lexical-binding: t; -*-

(after! org
  (setq-default org-babel-hide-result-overlays nil)
  ;; needed by poly-org-mode to be nil in all buffers
  (setq-default org-src-fontify-natively (if (featurep! :lang org +poly) nil t))
  ;; why would I need to indent src blocks?
  (setq-default org-edit-src-content-indentation 0))

(use-package! polymode
  :after-call polymode-minor-mode
  :config
  ;; we DONT want poly-org on every org buffer
  (setq auto-mode-alist (delete '("\\.org\\'" . poly-org-mode) auto-mode-alist))
  ;; enable poly org mode if "#+PROPERTY: poly:" is true
  (defun org-add-poly-mode-ensure-hook ()
    (if (member (cdr (assoc "poly:" org-keyword-properties)) '("yes" t))
        (poly-org-mode t)))
  (add-hook 'org-mode-hook #'org-add-poly-mode-ensure-hook))
(use-package! poly-markdown
  :after-call poly-markdown-mode
  :config
  ;; we DONT want poly-markdown on every org buffer
  (setq auto-mode-alist (delete '("\\.md\\'" . poly-markdown-mode) auto-mode-alist)))
(use-package! poly-org
  :after-call poly-org-mode
  :config
  ;; FIXME: force font-lock when inside a src block with poly mode
  (add-hook 'polymode-after-switch-buffer-hook
            (lambda (&rest _)
              (if poly-org-mode
                  (progn
                    ;; polymode resets this to nil..and is
                    ;; required by org-mode text properties
                    (setq org-src-fontify-natively t)
                    (font-lock-mode t)
                    (font-lock-ensure)))))
  ;; switch buffer from poly org mode we must ensure context
  ;; is within the host buffer
  (defun poly-avoid-indirect-buffer-history (wb)
    (let ((window (car wb))
          (bufs (cdr wb)))
      (let* ((b (let ((val (caar bufs)))
                  (if (listp val) (car val) val)))
             (bb (if b (buffer-base-buffer b) nil)))
        ;; only when buffer is indirect
        (if (and bb (not (eq b bb))
                 ;; only on poly mode
                 (buffer-local-value 'pm/polymode bb))
            (progn
              ;; prev buffers
              (if (listp (caar bufs))
                  (setf (car (caar bufs)) bb)
                ;; next buffers
                (setf (caar bufs) bb)))))
      (cons window bufs)))
  (advice-add #'set-window-prev-buffers :filter-args #'poly-avoid-indirect-buffer-history)
  (advice-add #'set-window-next-buffers :filter-args #'poly-avoid-indirect-buffer-history)

  ;; relay org buffer properties to polymode buffers
  (defun poly-org-buffer-properties (_ this-buf)
    (with-current-buffer (pm-base-buffer)
      (let ((properties org-keyword-properties))
        (with-current-buffer this-buf
          (setq-local org-keyword-properties properties)))))

  (pushnew! (oref pm-inner/org :switch-buffer-functions)
            #'poly-org-buffer-properties)
  ;; FIXME: org fails to parse src block language with poly-org
  ;; including the subsequent line in the language value
  ;; so we append a space at the end of each src block header
  ;; which makes the regex used by org work (look in the adviced fun)
  (defun org-babel-is-src-block-header-p ()
    (string-match "^[ \t]*#\\+BEGIN_SRC"
                  (substring-no-properties (thing-at-point 'line))))
  (defun org-ensure-header-trailing-space (&rest args)
    (if (org-babel-is-src-block-header-p)
        (let ((end (substring-no-properties (thing-at-point 'line) -2)))
          (if (not (string-match "\s+" end))
              (save-excursion
                (end-of-line)
                (insert " "))))))
  (advice-add #'org-element-src-block-parser :before #'org-ensure-header-trailing-space))

(if (featurep! :lang org +jupyter)
    (after! ob-jupyter
      (load! "jupyter")
      (require 'jupyter-org-client)
      (jupyter-org-interaction-mode t)
      (jupyter-repl-interaction-mode)))

(use-package! org-ref :commands org-ref-ivy-cite)
(use-package! ox-gfm :commands ox-gfm-export-to-markdown)
(use-package! org-pretty-tags :commands org-mode :after org)

(use-package! org-tanglesync
  :after ob
  :hook (org-mode . org-tanglesync-mode)
  :config
  (setq org-tanglesync-default-diff-action ':diff)
  (setq org-tanglesync-watch-files '())
  (org-tanglesync-watch-mode 1))

;; don't format org mode buffers on save
(if (featurep! :editor format +onsave)
    (add-to-list '+format-on-save-enabled-modes 'org-mode t))
;; enable lsp mode (without formatting) for org-mode if not using poly mode
(if (not (featurep! :lang org +poly))
    (add-hook 'org-mode-hook (lambda ()
                               (setq-local +format-with-lsp nil)
                               (lsp)
                               (lsp-org))))

;; (defun org-get-src-block-end ()
;;   (if (org-in-src-block-p)
;;       (re-search-forward
;;        (rx-to-string
;;         `(group
;;           bol
;;           (or
;;            (seq (one-or-more "*") space)
;;            (seq (zero-or-more (any " \t"))
;;                 "#+end"
;;                 ,(match-string 4)
;;                 word-end
;;                 (zero-or-more any)))))
;;        ;; We look further than LIMIT on purpose.
;;        nil t)
;;     (match-end 0)))
