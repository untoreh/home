;;; corfu.el -*- lexical-binding: t; -*-

(use-package! corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 0)
  (corfu-auto-delay 0.5)
  (corfu-echo-documentation 0.3)
  (corfu-quit-no-match 'separator)        ;; Automatically quit if there is no match
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match 'quit)
  :init
  (global-corfu-mode)

  :bind
  (:map corfu-map
   ("TAB" . corfu-next)
   ([tab] . corfu-next)
   ("C-n" . corfu-next)
   ("C-j" . corfu-insert)
   ("S-SPC" . corfu-insert-separator)
   ("S-TAB" . corfu-previous)
   ("C-p" . corfu-previous)
   ([?\r] . newline)
   ([backtab] . corfu-previous))
  :config
  (setq-default orderless-component-separator "[ &+-]")
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000)
  (setq tab-always-indent 'complete
        completion-cycle-threshold 3)
  (add-to-list 'corfu-auto-commands 'grammatical-edit-open-round)
  (add-to-list 'corfu-auto-commands 'grammatical-edit-open-bracket)
  (add-to-list 'corfu-auto-commands 'grammatical-edit-open-curly)

  (advice-add #'keyboard-quit :before #'corfu-quit)
  (add-to-list 'corfu-auto-commands 'end-of-visual-line)

  ;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  (evil-make-overriding-map corfu-map)

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  (use-package! corfu-quick
    :bind
    (:map corfu-map
     ("C-q" . corfu-quick-insert)))
  (with-eval-after-load 'all-the-icons
    (defvar kind-all-the-icons--cache nil
      "The cache of styled and padded label (text or icon).
An alist.")

    (defun kind-all-the-icons-reset-cache ()
      "Remove all cached icons from `kind-all-the-icons-mapping'."
      (interactive)
      (setq kind-all-the-icons--cache nil))

    (defun kind-all-the-icons--set-default-clear-cache (&rest args)
      (kind-all-the-icons-reset-cache)
      (apply #'set-default args))

    (defvar kind-all-the-icons--icons
      `((unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
        (text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
        (method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (fun . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (ctor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
        (variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
        (var . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
        (class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
        (interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (i/f . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (mod . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
        (prop . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
        (unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
        (value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
        (keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
        (k/w . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
        (snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
        (sn . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
        (color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
        (file . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
        (reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
        (ref . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
        (folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
        (dir . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
        (enum-member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
        (enummember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
        (member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
        (constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
        (const . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
        (struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
        (event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
        (operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
        (op . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
        (type-parameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
        (param . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
        (template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15))
        (t . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))))

    (defsubst kind-all-the-icons--metadata-get (metadata type-name)
      (or
       (plist-get completion-extra-properties (intern (format ":%s" type-name)))
       (cdr (assq (intern type-name) metadata))))

    (defun kind-all-the-icons-formatted (kind)
      "Format icon kind with all-the-icons"
      (or (alist-get kind kind-all-the-icons--cache)
          (let ((map (assq kind kind-all-the-icons--icons)))
            (let*  ((icon (if map
                              (cdr map)
                            (cdr (assq t kind-all-the-icons--icons))))
                    (half (/ (default-font-width) 2))
                    (pad (propertize " " 'display `(space :width (,half))))
                    (disp (concat pad icon pad)))
              (setf (alist-get kind kind-all-the-icons--cache) disp)
              disp))))

    (defun kind-all-the-icons-margin-formatter (metadata)
      "Return a margin-formatter function which produces kind icons.
METADATA is the completion metadata supplied by the caller (see
info node `(elisp)Programmed Completion').  To use, add this
function to the relevant margin-formatters list."
      (if-let ((kind-func (kind-all-the-icons--metadata-get metadata "company-kind")))
          (lambda (cand)
	    (if-let ((kind (funcall kind-func cand)))
	        (kind-all-the-icons-formatted kind)
	      (kind-all-the-icons-formatted t))))) ;; as a backup
    (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter)
    )
  ;; FIXME: basic completion style causes infinite recursion
  ;; (setq-hook! 'evil-insert-state-entry-hook
  ;;   completion-styles '(basic))
  ;; (setq-hook! 'evil-insert-state-exit-hook
  ;;   completion-styles '(orderless))
  )

(use-package! cape
  :after (corfu)
  :config
  (setq cape-dict-file (my/concat-path doom-cache-dir "spell" "aspell-dict.txt" ))
  (setq dabbrev-upcase-means-case-search t)
  (setq case-fold-search nil))

(use-package! tabnine-capf
  :after cape
  :commands (tabnine-completion-at-point)
  :hook (kill-emacs . tabnine-capf-kill-process)
  )

(use-package! corfu-doc
  :after corfu
  :hook (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
         ("M-d" . corfu-doc-toggle)))
(use-package copilot
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

(setq
 cape-symbol-scapf (cape-super-capf #'cape-keyword #'cape-symbol #'cape-abbrev)
 cape-word-scapf (cape-super-capf #'cape-ispell #'cape-dict)
 cape-expand-scapf (cape-super-capf #'cape-line) ;; snippets/tempel
 cape-file-scapf (cape-capf-buster
                  (cape-super-capf #'cape-dabbrev
                                   #'cape-file
                                   #'cape-history))
 cape-code-scapf (cape-super-capf #'cape-tex #'cape-sgml #'cape-rfc1345)
 cape-default-scapf (cape-capf-buster #'tabnine-completion-at-point)
 )
(defun my/super-capf-cmd! (scapf)
  (cmd! (let ((completion-at-point-functions (list scapf t)))
          (completion-at-point))))

(when (modulep! :tools lsp +eglot)
  (setq cape-eglot-scapf
        (cape-capf-buster (cape-super-capf #'tabnine-completion-at-point #'eglot-completion-at-point)))
  (setq-hook! 'eglot-managed-mode-hook
    completion-at-point-functions (list cape-eglot-scapf t)))
(setq-hook! nim-mode
  completion-at-point-functions (list #'tabnine-completion-at-point #'lsp-completion-at-point t))
(when (and (modulep! :tools lsp) (not (modulep! :tools lsp +eglot)))
  (setq cape-lsp-scapf
        (cape-super-capf #'tabnine-completion-at-point #'lsp-completion-at-point))
  (setq-hook! 'lsp-completion-mode-hook
    completion-at-point-functions (list cape-lsp-scapf t)))

(map! :after (corfu cape)
      (:desc "symbol" :i "C-SPC s" (my/super-capf-cmd! cape-symbol-scapf))
      (:desc "word" :i "C-SPC w" (my/super-capf-cmd! cape-word-scapf))
      (:desc "expand" :i "C-SPC d" (my/super-capf-cmd! cape-expand-scapf))
      (:desc "file" :i "C-SPC f" (my/super-capf-cmd! cape-file-scapf))
      (:desc "code" :i "C-SPC c" (my/super-capf-cmd! cape-code-scapf))
      (:desc "complete" :i "C-SPC SPC" (my/super-capf-cmd! cape-default-scapf))
      (:desc "complete" :i "C-SPC C-SPC" (my/super-capf-cmd! cape-default-scapf))
      (:mode lsp-mode
       (:desc "complete" :i "C-SPC SPC" (my/super-capf-cmd! cape-lsp-scapf))
       (:desc "complete" :i "C-SPC C-SPC" (my/super-capf-cmd! cape-lsp-scapf))
       ))
