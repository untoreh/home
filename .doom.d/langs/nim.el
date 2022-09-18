;;; langs/nim.el -*- lexical-binding: t; -*-

(map! :after nim-mode :mode nim-mode
      (:prefix ("SPC r" . "Nim REPL")
       :desc "focus and insert"
       :nev "i" #'nim-toggle-repl-and-insert
       :desc "exec region"
       :nev "e" #'nim-repl-send-region-or-line
       :desc "exec wrapped region"
       :nev "w" #'nim-repl-send-block-string
       :desc "exec src block"
       :nev "r" #'nim-repl-reset-and-execute-src-block
       :desc "list methods"
       :nev "m" #'nim-repl-list-methods
       :desc "list fields"
       :nev "f" #'nim-repl-list-fields
       :desc "exec function at point"
       :nev "l" #'nim-repl-send-function
       :desc "doc for expression"
       :nev "d" #'nim-repl-doc
       :desc "edit expression"
       :nev "v" #'nim-repl-edit
       :desc "toggle nim repl mode"
       :nev "t" #'nim-repl-mode)
      :mode nim-repl-vterm-mode
      (:desc "go to previous window"
       :nev "SPC w TAB" #'nim-toggle-repl-back))

(if (or t (modulep! :lang nim))
    (map! :after nim-repl
          (:prefix ("SPC l n" . "nim")
           :desc "start nim repl"
           :nev "r" (cmd! (nim-repl-switch))
           :nev "." #'nim-repl-cd
           :nev "d" #'nim-repl-toggle-debug
           :desc "toggle debug"
           :nev "D" (cmd! (my/toggle-env-var "NIM_DEBUG" "debug"))
           :desc "toggle local"
           :nev "l" (cmd! (my/toggle-env-var "NIM_LOCAL" "1"))
           :nev "v" #'nim-repl-revise
           )
          (:map 'nim-repl-mode-map
           "C-c C-p" nil
           "C-c C-v" nil
           "C-c ." #'nim-repl-cd
           :desc nil
           "C-c C-." #'nim-repl-cd)))

(use-package! nim-mode
  :if (or t (modulep! :lang nim))
  :init
  (put 'nim-compile-default-command 'safe-local-variable #'listp)
  :config
  (defun my/toggle-env-var (name on &optional off)
    (interactive)
    (if (getenv name)
        (progn
          (setenv name off)
          (message "Envvar OFF: %s=%s" name off))
      (setenv name on)
      (message "Envvar ON: %s=%s" name on)))
  (setq-hook! 'nim-compile-mode-hook
     before-change-functions (append before-change-functions '(my/tail-f-window)))
  )
(setq list-1 '(a b c))
(append list-1 '(d))
;; (when (modulep! :tools lsp +eglot)
;;   (after! eglot
;;           (add-to-list 'eglot-server-programs '(nim-mode "nimlsp"))))
(when (or t modulep! :lang nim +lsp)
  (setq lsp-nim-project-mapping [(:projectFile "main.nim" :fileRegex ".*\\.nim")
                                 (:projectFile "test.nim" :fileRegex "test.*\\.nim")])
  (add-hook! nim-mode #'lsp (pyvenv-mode 1))
  (setq-hook! nim-mode
    lsp-auto-guess-root t
    ;; lsp-completion-enable nil
    flycheck-checker-error-threshold 100000))

(setq nim-indent-offset 2)
(setq-hook! 'nim-mode-hook
  evil-shift-width 2)

(setq-default
 nim-compile-command "nim"
 nim-compile-default-args '("r" "-r" "--verbosity:0" "--hint[Processing]:off" "--excessiveStackTrace:on")
 nim-compile-default-command '("r")
 nimsuggest-options '("--refresh" "--maxresults:10")
 )

(map! :mode (nim-mode nimscript-mode)
      :leader
      :prefix "c"
      :nev "c" #'nim-compile)
;; disable nimsuggest since using LSP
;; (remove-hook! 'nim-mode-hook #'+nim-init-nimsuggest-mode-h)

(defun nim-repl-toggle-debug () (error "Not implemented."))


(use-package! nim-repl
  :config
  (nim-repl-set-terminal-backend 'vterm)
  (setq nim-repl-switches "--withTools -d:--threads:on")
  (add-hook! nim-mode #'nim-repl-mode))

(defvar nim-format-line-len "100"
  "Line length argument for nimpretty.")

(after! format-all
  (defun nim-mode-format ()
    (interactive)
    (let ((tmpfile (make-temp-file "nim" nil ".nim")))
      (unwind-protect
          (progn
            (write-file tmpfile)
            (call-process
             "nimfmt" nil nil nil tmpfile "-i" tmpfile
             ;; if nimpretty
             ;; "nimpretty" nil nil nil
             ;; "--indent:" (number-to-string  nim-indent-offset)
             ;; "--maxLineLen:" nim-format-line-len
             )
            (erase-buffer)
            (insert-file-contents tmpfile)
            )
        (delete-file tmpfile))))
  (set-formatter! 'nimfmt #'nim-mode-format :modes '(nim-mode)))

(after! nim-mode
  ;; (put 'nim-compile-default-command 'risky-local-variable nil)
  )

(add-hook! 'nim-mode-hook :depth 0
  (setq +lookup-definition-functions
        (delete #'+nimsuggest-find-definition +lookup-definition-functions)))

(setq-hook! nim-mode
  devdocs-current-docs '("nim")
  dumb-jump-default-project "~/.nimble/pkgs")

(set-popup-rules!
  '(("^\\*nim-compile" :height 25 :quit t :select nil)
    ("^\\*doom eval" :select t :quit t :ttl 1))
  )
