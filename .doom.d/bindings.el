;;; ../../../var/home/fra/.doom.d/bindings.el -*- lexical-binding: t; -*-
;;;
(setq-default evil-escape-key-sequence "fd")
;; dap
;; ( "jm" #'dumb-jump-go-prompt)
;; ( "mdi" #'dap-step-in)
;; ( "mdo" #'dap-step-out)
;; ( "mdp" #'dap-ui-inspect-thing-at-point)

;; ivy
(setq-default ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

;; common
(map! :leader
      :desc "find file at point"
      :prefix "f"
      :n "."
      #'find-file-at-point)

;; parrot
(map!
 :mode parrot-mode
 :n "[r" #'parrot-rotate-prev-word-at-point
 :n "]r" #'parrot-rotate-next-word-at-point)

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
      (set-marker p nil)))
  (require 'ivy-hydra)
  (defhydra hydra/org-babel (:hint nil :color amaranth)
    "
[_<return>_]: exec block
[_n_]: next block       [_C-n_]: exec next        [_._]: exec to point
[_p_]: previous block   [_C-p_]: exec previous    [_b_]: exec buffer
[_<tab>_]: (un)fold     [_x_]: kill block         [_c_]: clone
[_s_]: split            [_m_]: merge              [_q_]: quit
"
    ("<return>" org-babel-execute-src-block)
    ("n" org-babel-next-src-block)
    ("P" org-babel-next-src-block)
    ("p" org-babel-previous-src-block)
    ("N" org-babel-previous-src-block)
    ("x" jupyter-org-kill-block-and-results)
    ("C-n" org-babel-exec-next-block)
    ("C-p" org-babel-exec-previous-block)

    ("s" jupyter-org-split-src-block)
    ("m" jupyter-org-merge-blocks)
    ("c" jupyter-org-clone-block)

    ("." org-babel-exec-to-point :color teal)
    ("b" org-babel-execute-buffer :color teal)
    ("<tab>" org-cycle :color yellow)
    ("q" nil)
    ("C-g" nil)))

(map! :after evil-org
      :map evil-org-mode-map
      :leader
      :desc "tangle" :n "ct" #'org-babel-tangle
      :desc "hydra org babel" :n "," #'hydra/org-babel/body)

(map! :after org
      (:leader
       :prefix ("io" . "org")
       :mode org-mode
       :desc "src block name"
       :n "n" #'(lambda (&rest args) (interactive)
                  (if (org-in-src-block-p)
                      (progn
                        (org-babel-goto-src-block-head)
                        (previous-line)
                        (newline)))
                  (insert "#+NAME: ")
                  (evil-insert nil))
       :desc "src block header arg"
       :n "i" #'org-babel-insert-header-arg))

;; julia
(map! :after julia-mode :mode julia-mode
      (:prefix ("SPC r" . "Julia REPL")
       :desc "focus and insert"
       :nv "i" #'julia-toggle-repl-and-insert
       :desc "exec region"
       :nv "e" #'julia-repl-send-region-or-line
       :desc "exec wrapped region"
       :nv "w" #'julia-repl-send-block-string
       :desc "exec src block"
       :nv "r" #'julia-repl-reset-and-execute-src-block
       :desc "list methods"
       :nv "m" #'julia-repl-list-methods
       :desc "list fields"
       :nv "f" #'julia-repl-list-fields
       :desc "exec function at point"
       :nv "l" #'julia-repl-send-function
       :desc "doc for expression"
       :nv "d" #'julia-repl-doc
       :desc "edit expression"
       :nv "v" #'julia-repl-edit
       :desc "toggle julia repl mode"
       :nv "t" #'julia-repl-mode)
      :mode julia-repl-vterm-mode
      (:desc "go to previous window"
       :nv "SPC w TAB" #'julia-toggle-repl-back))

(if (featurep! :lang julia)
    (map! (:prefix ("SPC l j" . "julia")
           :desc "start julia repl"
           :nv "r" #'julia-repl
           :nv "f" #'julia-franklin
           :nv "." #'julia-repl-cd
           )))

;; unbind redundant bindings
(if (featurep! :editor evil)
    (progn
      (map! (:map 'global
             "C-w" nil
             "C-h" nil)
            (:map 'evil-motion-state-map
             "C-w" nil
             "C-h" nil))))

;; misc
;; insert triple quotes
;; (map!
;;  :when (featurep! :editor evil)
;;  :leader
;;  :desc "triple quotes"
;;  "q" (lambda () (evil-insert "\"\"\"\"\"\"")))

;; still want to escape from terminals
(after! evil-escape
  (setq evil-escape-excluded-major-modes
        (delete 'vterm-mode evil-escape-excluded-major-modes )))
(map! :leader
      :desc "Re-open the current file"
      :n "bR" #'save-close-reopen-file)
(map! :after hydra
      :leader
      :desc "hydra windows"
      :n "w ," #'+hydra/window-nav/body)

(map! :mode gif-screencast-mode
      :desc "start recording"
      "<f9>" #'gif-screencast-start-or-stop)

(map! :mode org-mode
      :leader
      :desc "font lock ensure on"
      :n "t t" (lambda () (interactive)
                 (font-lock-mode t)
                 (font-lock-ensure)
                 (font-lock-mode t)))

;; smartparens toggle motion beginning and end of current bracket enclosing;
;; we don't want to move within quotes so delete them from smartparens pairs
;; and add them back after motion
(setq sp-no-motion-list '(wrap insert autoskip navigate escape))
(defun sp-add-quotes nil
  (sp-pair "\"" "\"")
  (sp-pair "\\\"" "\\\"")
  (sp-pair "'" "'"))
(defun sp-rem-quotes nil
  (sp-pair "\"" "\"" :actions sp-no-motion-list)
  (sp-pair "\\\"" "\\\"" :actions sp-no-motion-list)
  (sp-pair "'" "'" :actions sp-no-motion-list))

(defvar-local my/back-and-forth-paren-state nil "tracks the toggle state of back and forth paren")
(defun my/move-to-current-parent-toggle nil
  (interactive)
  (unwind-protect
      (if my/back-and-forth-paren-state
          (progn
            (sp-rem-quotes)
            (sp-beginning-of-sexp)
            (sp-add-quotes)
            (setq my/back-and-forth-paren-state nil))
        (progn
          (sp-rem-quotes)
          (sp-end-of-sexp)
          (evil-backward-char)
          (sp-add-quotes)
          (setq my/back-and-forth-paren-state t)))
    (sp-add-quotes)))

(map! :n "g \\" nil)
(map! :desc "Back and forth between current paren enclosing"
      :n "g \\" #'my/move-to-current-parent-toggle)

;; wechat
(map! :desc "Start weechat"
      :leader
      :n "o c"
      (lambda ()
        (interactive )
        (when (not (weechat-connected-p))
          (weechat-connect "localhost" 9000 nil 'plain t))
        (weechat-switch-buffer
         (first (list (weechat--read-channel-name (not current-prefix-arg)))))))

;; magit doesn't ship this option
(after! magit
  (transient-append-suffix 'magit-merge "-A"
    '("-A" "Allow unrelated histories" "--allow-unrelated-histories")))
