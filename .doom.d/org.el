;;; ../../../var/home/fra/.doom.d/org.el -*- lexical-binding: t; -*-

(setq org-babel-hide-result-overlays nil)
;; needed by poly-org-mode to be nil in all buffers
(setq org-src-fontify-natively (if (featurep! :lang org +poly) nil t))
;; why would I need to indent src blocks?
(setq org-edit-src-content-indentation 0)

(use-package! polymode
  :after-call polymode-minor-mode
  :config
  ;; enable poly org mode if "#+PROPERTY: poly:" is true
  (defun org-add-poly-mode-ensure-hook ()
    (if (member (cdr (assoc "poly:" org-keyword-properties)) '("yes" t))
        (poly-org-mode 1)))
  (add-hook 'org-mode-hook #'org-add-poly-mode-ensure-hook))
(use-package! poly-markdown
  :init
  ;; we DONT want poly-markdown on every org buffer
  (setq auto-mode-alist (delete '("\\.md\\'" . poly-markdown-mode) auto-mode-alist))
  :config
  ;; we DONT want poly-markdown on every org buffer
  (setq auto-mode-alist (delete '("\\.md\\'" . poly-markdown-mode) auto-mode-alist))
  :after-call poly-markdown-mode)
(use-package! poly-org
  :init
  ;; we DONT want poly-org on every org buffer
  (setq auto-mode-alist (delete '("\\.org\\'" . poly-org-mode) auto-mode-alist))
  :after-call poly-org-mode
  :config
  ;; we DONT want poly-org on every org buffer
  (setq auto-mode-alist (delete '("\\.org\\'" . poly-org-mode) auto-mode-alist))
  ;; FIXME: force font-lock when inside a src block with poly mode
  (add-hook 'polymode-after-switch-buffer-hook
            (lambda (&rest _)
              (if poly-org-mode
                  (progn
                    ;; temporarily enable native fontification since
                    ;; required by org-mode text properties
                    (setq org-src-fontify-natively t)
                    (font-lock-mode 1)
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
      (jupyter-org-interaction-mode 1)
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

;; NOTE: consider `org-roam' if the need for it arises

;; fail org mode hooks functions gracefully
(defadvice! shut-up-org-problematic-hooks (orig-fn &rest args)
  :around #'org-fancy-priorities-mode
  :around #'org-superstar-mode
  (ignore-errors (apply orig-fn args)))

;; Theming, all from tecosaur
(add-hook 'org-mode-hook #'+org-pretty-mode)
(custom-set-faces!
  '(outline-1 :weight extra-bold :height 1.5)
  '(outline-2 :weight bold :height 0.93)
  '(outline-3 :weight bold :height 0.93)
  '(outline-4 :weight semi-bold :height 0.93)
  '(outline-5 :weight semi-bold :height 0.93)
  '(outline-6 :weight semi-bold :height 0.93)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))
(custom-set-faces!
  '(org-document-title :height 1.2))

(setq org-fontify-quote-and-verse-blocks t)

;; TODO: try this in other places that feel slow
(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))
(add-hook 'org-mode-hook #'locally-defer-font-lock)

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-superstar-prettify-item-bullets t ))

(setq org-ellipsis " ▾ "
      org-hide-leading-stars t
      org-priority-highest ?A
      org-priority-lowest ?E
      org-priority-faces
      '((?A . 'all-the-icons-red)
        (?B . 'all-the-icons-orange)
        (?C . 'all-the-icons-yellow)
        (?D . 'all-the-icons-green)
        (?E . 'all-the-icons-blue)))

(appendq! +ligatures-extra-symbols
          `(:checkbox      "☐"
            :pending       "◼"
            :checkedbox    "☑"
            :list_property "∷"
            :em_dash       "—"
            :ellipses      "…"
            :arrow_right   "→"
            :arrow_left    "←"
            :title         "𝙏"
            :subtitle      "𝙩"
            :author        "𝘼"
            :date          "𝘿"
            :property      "☸"
            :options       "⌥"
            :startup       "⏻"
            :macro         "𝓜"
            :html_head     "🅷"
            :html          "🅗"
            :latex_class   "🄻"
            :latex_header  "🅻"
            :beamer_header "🅑"
            :latex         "🅛"
            :attr_latex    "🄛"
            :attr_html     "🄗"
            :attr_org      "⒪"
            :begin_quote   "❝"
            :end_quote     "❞"
            :caption       "☰"
            :header        "›"
            :results       "🠶"
            :begin_export  "⏩"
            :end_export    "⏪"
            :properties    "⚙"
            :end           "∎"
            :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
            :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
            :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
            :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
            :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)))
(set-ligatures! 'org-mode
  :merge t
  :checkbox      "[ ]"
  :pending       "[-]"
  :checkedbox    "[X]"
  :list_property "::"
  :em_dash       "---"
  :ellipsis      "..."
  :arrow_right   "->"
  :arrow_left    "<-"
  :title         "#+title:"
  :subtitle      "#+subtitle:"
  :author        "#+author:"
  :date          "#+date:"
  :property      "#+property:"
  :options       "#+options:"
  :startup       "#+startup:"
  :macro         "#+macro:"
  :html_head     "#+html_head:"
  :html          "#+html:"
  :latex_class   "#+latex_class:"
  :latex_header  "#+latex_header:"
  :beamer_header "#+beamer_header:"
  :latex         "#+latex:"
  :attr_latex    "#+attr_latex:"
  :attr_html     "#+attr_html:"
  :attr_org      "#+attr_org:"
  :begin_quote   "#+begin_quote"
  :end_quote     "#+end_quote"
  :caption       "#+caption:"
  :header        "#+header:"
  :begin_export  "#+begin_export"
  :end_export    "#+end_export"
  :results       "#+RESULTS:"
  :property      ":PROPERTIES:"
  :end           ":END:"
  :priority_a    "[#A]"
  :priority_b    "[#B]"
  :priority_c    "[#C]"
  :priority_d    "[#D]"
  :priority_e    "[#E]")
(plist-put +ligatures-extra-symbols :name "⁍")


(after! org-plot
  (defun org-plot/generate-theme (_type)
    "Use the current Doom theme colours to generate a GnuPlot preamble."
    (format "
fgt = \"textcolor rgb '%s'\" # foreground text
fgat = \"textcolor rgb '%s'\" # foreground alt text
fgl = \"linecolor rgb '%s'\" # foreground line
fgal = \"linecolor rgb '%s'\" # foreground alt line

# foreground colors
set border lc rgb '%s'
# change text colors of  tics
set xtics @fgt
set ytics @fgt
# change text colors of labels
set title @fgt
set xlabel @fgt
set ylabel @fgt
# change a text color of key
set key @fgt

# line styles
set linetype 1 lw 2 lc rgb '%s' # red
set linetype 2 lw 2 lc rgb '%s' # blue
set linetype 3 lw 2 lc rgb '%s' # green
set linetype 4 lw 2 lc rgb '%s' # magenta
set linetype 5 lw 2 lc rgb '%s' # orange
set linetype 6 lw 2 lc rgb '%s' # yellow
set linetype 7 lw 2 lc rgb '%s' # teal
set linetype 8 lw 2 lc rgb '%s' # violet

# border styles
set tics out nomirror
set border 3

# palette
set palette maxcolors 8
set palette defined ( 0 '%s',\
1 '%s',\
2 '%s',\
3 '%s',\
4 '%s',\
5 '%s',\
6 '%s',\
7 '%s' )
"
            (doom-color 'fg)
            (doom-color 'fg-alt)
            (doom-color 'fg)
            (doom-color 'fg-alt)
            (doom-color 'fg)
            ;; colours
            (doom-color 'red)
            (doom-color 'blue)
            (doom-color 'green)
            (doom-color 'magenta)
            (doom-color 'orange)
            (doom-color 'yellow)
            (doom-color 'teal)
            (doom-color 'violet)
            ;; duplicated
            (doom-color 'red)
            (doom-color 'blue)
            (doom-color 'green)
            (doom-color 'magenta)
            (doom-color 'orange)
            (doom-color 'yellow)
            (doom-color 'teal)
            (doom-color 'violet)
            ))
  (defun org-plot/gnuplot-term-properties (_type)
    (format "background rgb '%s' size 1050,650"
            (doom-color 'bg)))
  (setq org-plot/gnuplot-script-preamble #'org-plot/generate-theme)
  (setq org-plot/gnuplot-term-extra #'org-plot/gnuplot-term-properties))
