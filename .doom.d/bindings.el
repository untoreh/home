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
(when (modulep! :tools magit)
  (map! :desc "amend commit"
        :after magit
        :leader
        :prefix "g"
        :n "a"
        (cmd! (magit-commit-amend)))
  ;; magit doesn't ship this option
  (after! magit
    (transient-append-suffix 'magit-merge "-A"
      '("-A" "Allow unrelated histories" "--allow-unrelated-histories"))))

(map! :leader
      (:desc "Find file at point."
       :prefix "f"
       :n "."
       #'find-file-at-point
       :desc "Toggle iedit mode."
       :prefix "t"
       :nv "e"
       #'iedit-mode
       ;; doom kill buffer doesn't seem to kill dead processes buffers
       :prefix "b" "k"
       #'my/force-kill-buffer
       :desc "Toggle font lock"
       :prefix "t"
       :nv "j"
       #'font-lock-mode
       )
      )

;; parrot
(after! parrot-mode
  (map!
   :nve "[r" #'parrot-rotate-prev-word-at-point
   :nve "]r" #'parrot-rotate-next-word-at-point
   :nve
   ))

;; jupyter
(if (modulep! :lang org +jupyter)
    (map!
     :after org
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

;; HYDRA
(use-package! hydra
  :commands (hydra/window-nav/body hydra-macro/body)
  :config
  (load! "hydra")
  (map!
   :after hydra
   :leader
   :desc "hydra windows"
   :n "w ," #'+hydra/window-nav/body
   :n "q ," #'hydra-macro/body
   ))


(map! :after calc
      :desc "Quick Calc"
      :leader
      :nev "o c" #'quick-calc)

;; JULIA
(map! :after julia-mode
      :mode julia-mode
      (:prefix ("SPC r" . "Julia REPL")
       :desc "focus and insert"
       :nev "i" #'julia-toggle-repl-and-insert
       :desc "exec region"
       :nev "e" #'julia-repl-send-region-or-line
       :desc "exec wrapped region"
       :nev "b" #'julia-repl-send-block-string
       :desc "exec src block"
       :nev "r" #'julia-repl-revise
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

(if (modulep! :lang julia)
    (map! :after julia-mode
          (:prefix ("SPC l j" . "julia")
           :desc "start julia repl"
           :nev "r" (cmd! (let ((julia-repl-enable-revise nil)) (julia-repl-switch)))
           :nev "." #'julia-repl-cd
           :nev "d" #'julia-repl-toggle-debug
           (:prefix ("v" . "revise")
            :nev "p" #'julia-repl-revise-project
            :nev "." #'julia-repl-revise-at-point)
           :nev "j" #'julia-repl-startup
           (:prefix ("f" . "franklin")
            :nev "f" #'julia-franklin
            :desc "Setup julia frankling env"
            :nev "y" #'julia-franklin-sync-blog
            :desc "Sync __site dir over to tmp dir"
            :nev "u" #'julia-franklin-publish
            :nev "s" #'julia-franklin-serve
            :nev "k" #'julia-franklin-stop
            )
           (:prefix ("p" . "packages")
            :desc "Pkg precompile"
            :nev "p" (cmd! (julia-repl-precompile))
            :desc "Main env load path"
            :nev "d" (cmd! (julia-repl-debug-packages))
            :desc "Toggle proto structures"
            :nev "s" (cmd! (julia-toggle-proto-structs))
            :desc "Run tests"
            :nev "t" #'julia-repl-run-tests
            ))
          (:map 'julia-repl-mode-map
           "C-c C-p" nil
           "C-c C-v" nil
           "C-c ." #'julia-repl-cd
           :desc nil
           "C-c C-." #'julia-repl-cd)))

;; evil rebindings
(if (modulep! :editor evil)
    (progn
      (map! (:map 'global
                  "C-w" nil
                  "C-h" nil)
            (:map 'evil-motion-state-map
                  "C-w" nil
                  "C-h" nil))))

;; evil align lines
(map!
 :after evil-lion
 :n "g a" #'evil-lion-left
 :n "g A" #'evil-lion-right
 :n "g l" nil
 :n "g L" nil)

;; evil bindings in the minibuffer
;; (setq evil-collection-setup-minibuffer t)

;; still want to escape from terminals
(after! evil-escape
  (setq evil-escape-excluded-major-modes
        (delete 'vterm-mode evil-escape-excluded-major-modes )))

(map! :leader
      :desc "Re-open the current file"
      :n "bR" #'save-close-reopen-file)

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

