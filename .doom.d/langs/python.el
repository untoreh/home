;; ;; livepy
;; (add-hook 'live-py-mode-hook
;;           (lambda ()
;;             (progn
;;               (setq-default live-py-version (executable-find "python"))
;;               (live-py-update-all))))

(map! :after python-mode :mode python-mode
      (:prefix ("SPC r" . "Python REPL")
       :desc "focus and insert"
       :nev "i" #'python-toggle-repl-and-insert
       :desc "exec region"
       :nev "e" #'python-repl-send-region-or-line
       :desc "exec wrapped region"
       :nev "w" #'python-repl-send-block-string
       :desc "exec src block"
       :nev "r" #'python-repl-reset-and-execute-src-block
       :desc "list methods"
       :nev "m" #'python-repl-list-methods
       :desc "list fields"
       :nev "f" #'python-repl-list-fields
       :desc "exec function at point"
       :nev "l" #'python-repl-send-function
       :desc "doc for expression"
       :nev "d" #'python-repl-doc
       :desc "edit expression"
       :nev "v" #'python-repl-edit
       :desc "toggle python repl mode"
       :nev "t" #'python-repl-mode)
      :mode python-repl-vterm-mode
      (:desc "go to previous window"
       :nev "SPC w TAB" #'python-toggle-repl-back))

(if (featurep! :lang python)
    (map! :after python-repl
          (:prefix ("SPC l p" . "python")
           :desc "start python repl"
           :nev "r" (cmd! (python-repl-switch))
           :nev "." #'python-repl-cd
           :nev "d" #'python-repl-toggle-debug
           :nev "v" #'python-repl-revise
           )
          (:map 'python-repl-mode-map
           "C-c C-p" nil
           "C-c C-v" nil
           "C-c ." #'python-repl-cd
           :desc nil
           "C-c C-." #'python-repl-cd)))


(defun python-repl-toggle-debug () (error "Not implemented."))

;; (use-package! ein
;;   :config
;;   (setq-default
;;    ;; allow whole cells undo
;;    ein:worksheet-enable-undo nil
;;    ;; multi major mode support
;;    ein:polymode t)
;;   (custom-set-faces
;;    '(ein:cell-input-area ((t (:background "#262833"))))))

(after! python-mode
  :config
  ;; make isort compatible with black
  (setq-default py-isort-options '("--profile" "black"))
  (add-to-list '+format-on-save-enabled-modes 'python-mode t)
  (add-hook 'python-mode-hook #'python-repl-mode)

  (setq
   ;; FIXME complains about the python interpreter not supporting completion...
   python-shell-completion-native-enable nil
   ;; python-shell-interpreter "python"
   ;; FIXME https://github.com/jorgenschaefer/elpy/issues/1744
   +python-jupyter-command '("jupyter" "console")
   )
  ;; use jupyter repl as default python repl
  (setf (alist-get 'python-mode +eval-repls)
        '(+python/open-jupyter-repl :persist t)
        ))

(use-package! python-repl
  :config
  (python-repl-set-terminal-backend 'vterm))

;; NOTE: pyvenv-activate needs the path the virtual env directory (usually .venv or .env)
