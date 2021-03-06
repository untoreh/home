;;; ../../../var/home/fra/.doom.d/bindings.el -*- lexical-binding: t; -*-
;;;
(setq-default evil-escape-key-sequence "fd")

;; convenience
(map! :leader
      "h y" #'describe-keymap)


;; which key configs
;; TODO: this doesn't look ideal:
;; - whichkey should cache entries such that replacements are only applied
;;   when a keymap is updated
;; - a package should collect all the replacements and standardize the
;;   ("long-name" => icon) pair
;; (setq which-key-allow-multiple-replacements t)
;; (after! which-key
;;   (pushnew!
;;    which-key-replacement-alist
;;    '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
;;    '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
;;    ))
;; TODO: a couple nice improvements for which-key
;; - colorized entries based on the package/library they belong to
;; - replace "prefix" entries with some form of previews

;; dap
;; ( "jm" #'dumb-jump-go-prompt)
;; ( "mdi" #'dap-step-in)
;; ( "mdo" #'dap-step-out)
;; ( "mdp" #'dap-ui-inspect-thing-at-point)

;; magit
(when (featurep! :tools magit)
  (map! :desc "amend commit"
        :after (:tools magit)
        :leader
        :prefix "g"
        :n "a"
        (cmd! (magit-commit-amend)))
  ;; magit doesn't ship this option
  (after! magit
    (transient-append-suffix 'magit-merge "-A"
      '("-A" "Allow unrelated histories" "--allow-unrelated-histories"))))

;; common
(map! :leader
      :desc "find file at point"
      :prefix "f"
      :n "."
      #'find-file-at-point)

;; inserting inside a vterm should reset cursor position
(map! :mode vterm-mode
      :n "o" (cmd!
              (vterm-reset-cursor-point)
              (evil-collection-vterm-insert)))

;; parrot
(after! parrot-mode
  (map!
   :after parrot
   :n "[r" #'parrot-rotate-prev-word-at-point
   :n "]r" #'parrot-rotate-next-word-at-point))

;; jupyter
(if (featurep! :lang org +jupyter)
    (map!
     :after evil-org
     :when use-jupyter
     :mode org-mode
     :map evil-org-mode-map
     :leader
     :desc "tangle" :n "ct" #'org-babel-tangle
     :localleader
     :desc "Hydra" :n "," #'jupyter-org-hydra/body
     :desc "Inspect at point" :n "?" #'jupyter-inspect-at-point
     :desc "Execute and step" :n "RET" #'jupyter-org-execute-and-next-block
     :desc "Delete code block" :n "x" #'jupyter-org-kill-block-and-results
     :desc "New code block above" :n "+" #'jupyter-org-insert-src-block
     :desc "New code block below" :n "=" (λ! () (interactive) (jupyter-org-insert-src-block t nil))
     :desc "Merge code blocks" :n "m" #'jupyter-org-merge-blocks
     :desc "Split code block" :n "-" #'jupyter-org-split-src-block
     :desc "Fold results" :n "z" #'org-babel-hide-result-toggle
     :desc "Restart kernels" :n "k"
     #'(λ! () (interactive)
          (jupyter-org-with-src-block-client (jupyter-repl-restart-kernel)))
     :map org-src-mode-map
     :localleader
     :desc "Exit edit" :n "'" #'org-edit-src-exit))

;; evil align lines
(map!
 :after evil-lion
 :n "g a" #'evil-lion-left
 :n "g A" #'evil-lion-right
 :n "g l" nil
 :n "g L" nil)

;; evil bindings in the minibuffer
;; (setq evil-collection-setup-minibuffer t)

;; org
(after! evil-org
  (defun org-babel-exec-next-block ()
    (interactive)
    (org-babel-execute-src-block)
    (org-babel-next-src-block))
  (defun org-babel-exec-previous-block ()
    (interactive)
    (org-babel-execute-src-block)
    (org-babel-previous-src-block))
  (defun org-babel-exec-to-point ()
    (interactive)
    (let ((p (point-marker)))
      (set-marker-insertion-type p t)
      (catch 'done
        (org-babel-map-src-blocks nil
          (progn
            (when (> (point) p)
              (throw 'done t))
            (org-babel-execute-src-block))))
      (goto-char p)
      (set-marker p nil))))

(map! :after evil-org
      :map evil-org-mode-map
      :leader
      :desc "tangle" :n "ct" #'org-babel-tangle
      :desc "hydra org babel" :n "," #'hydra/org-babel/body)

;; HYDRA
(after! hydra (load! "hydra"))

(map! :after hydra
      :leader
      :desc "hydra windows"
      :n "w ," #'+hydra/window-nav/body
      :n "q ," #'hydra-macro/body
      )

(map! :after org
      (:leader
       :prefix ("io" . "org")
       :mode org-mode
       :desc "src block name"
       :n "n" (cmd!
               (if (org-in-src-block-p)
                   (progn
                     (org-babel-goto-src-block-head)
                     (previous-line)
                     (newline)))
               (insert "#+NAME: ")
               (evil-insert nil))
       :desc "src block header arg"
       :n "i" #'org-babel-insert-header-arg))

;; JULIA
(map! :after julia-mode :mode julia-mode
      (:prefix ("SPC r" . "Julia REPL")
       :desc "focus and insert"
       :nev "i" #'julia-toggle-repl-and-insert
       :desc "exec region"
       :nev "e" #'julia-repl-send-region-or-line
       :desc "exec wrapped region"
       :nev "w" #'julia-repl-send-block-string
       :desc "exec src block"
       :nev "r" #'julia-repl-reset-and-execute-src-block
       :desc "list methods"
       :nev "m" #'julia-repl-list-methods
       :desc "list fields"
       :nev "f" #'julia-repl-list-fields
       :desc "exec function at point"
       :nev "l" #'julia-repl-send-function
       :desc "doc for expression"
       :nev "d" #'julia-repl-doc
       :desc "edit expression"
       :nev "v" #'julia-repl-edit
       :desc "toggle julia repl mode"
       :nev "t" #'julia-repl-mode)
      :mode julia-repl-vterm-mode
      (:desc "go to previous window"
       :nev "SPC w TAB" #'julia-toggle-repl-back))

(if (featurep! :lang julia)
    (map! :after julia-repl
          (:prefix ("SPC l j" . "julia")
           :desc "start julia repl"
           :nev "r" (cmd! (julia-repl-switch))
           :nev "f" #'julia-franklin
           :nev "k" #'julia-franklin-stop
           :nev "." #'julia-repl-cd
           :nev "d" #'julia-repl-toggle-debug
           :nev "v" #'julia-repl-revise
           :nev "y" #'julia-franklin-sync-blog
           )
          (:map 'julia-repl-mode-map
           "C-c C-p" nil
           "C-c C-v" nil
           "C-c ." #'julia-repl-cd
           :desc nil
           "C-c C-." #'julia-repl-cd)))

;; evil rebindings
(if (featurep! :editor evil)
    (progn
      (map! (:map 'global
             "C-w" nil
             "C-h" nil)
            (:map 'evil-motion-state-map
             "C-w" nil
             "C-h" nil))))

;; still want to escape from terminals
(after! evil-escape
  (setq evil-escape-excluded-major-modes
        (delete 'vterm-mode evil-escape-excluded-major-modes )))

(map! :leader
      :desc "Re-open the current file"
      :n "bR" #'save-close-reopen-file)

(map! :mode org-mode
      :leader
      :desc "font lock ensure on"
      :n "t t" (cmd!
                (font-lock-mode 1)
                (font-lock-ensure)
                (font-lock-mode 1)))

;; smartparens toggle motion beginning and end of current bracket enclosing;
;; we don't want to move within quotes so delete them from smartparens pairs
;; and add them back after motion
(when (bound-and-true-p smartparens-global-mode)
  (setq sp-no-motion-list '(wrap insert autoskip navigate escape))
  (comp-defun sp-add-quotes nil
              (sp-pair "\"" "\"")
              (sp-pair "\\\"" "\\\"")
              (sp-pair "'" "'"))
  (comp-defun sp-rem-quotes nil
              (sp-pair "\"" "\"" :actions sp-no-motion-list)
              (sp-pair "\\\"" "\\\"" :actions sp-no-motion-list)
              (sp-pair "'" "'" :actions sp-no-motion-list))

  (comp-defun my/move-to-current-parent-toggle nil
              (interactive)
              (sp-rem-quotes)
              (let* ((inhibit-redisplay t)
                     (p (point))
                     (bgn (save-excursion
                            (sp-beginning-of-sexp)
                            (equal p (point)))))
                (if bgn
                    (sp-end-of-sexp)
                  (sp-beginning-of-sexp)))
              (sp-add-quotes))

  (map! :desc "Back and forth between current paren enclosing"
        :n "g \\" #'my/move-to-current-parent-toggle))

;; weechat
(map! :desc "Start weechat"
      :leader
      :nev "o c"
      (cmd!
       (when (not (weechat-connected-p))
         (weechat-connect "localhost" 9000 nil 'plain t)
         (weechat-auto-monitor))
       (weechat-switch-buffer
        (first (list (weechat--read-channel-name (not current-prefix-arg)))))))

