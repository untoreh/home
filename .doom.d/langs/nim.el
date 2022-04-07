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

(if (or t (featurep! :lang nim))
    (map! :after nim-repl
          (:prefix ("SPC l n" . "nim")
           :desc "start nim repl"
           :nev "r" (cmd! (nim-repl-switch))
           :nev "." #'nim-repl-cd
           :nev "d" #'nim-repl-toggle-debug
           :nev "v" #'nim-repl-revise
           )
          (:map 'nim-repl-mode-map
           "C-c C-p" nil
           "C-c C-v" nil
           "C-c ." #'nim-repl-cd
           :desc nil
           "C-c C-." #'nim-repl-cd)))
(use-package! nim-mode
  :if (or t (featurep! :lang nim))
  :init
  (put 'nim-compile-default-command 'safe-local-variable #'listp)
  :config
  (if (or t (featurep! :lang nim +lsp))
      (add-hook! nim-mode #'lsp))

  (setq nim-indent-offset 4)
  (setq-hook! 'nim-mode-hook
    evil-shift-width 4)

  (setq-default
   nim-compile-command "nimwrap"
   nim-compile-default-command
   '("r" "-r" "--verbosity:0" "--hint[Processing]:off" "--excessiveStackTrace:on")
   nimsuggest-options '("--refresh" "--maxresults:10"))

  (map! :mode (nim-mode nimscript-mode)
        :leader
        :prefix "c"
        :nev "c" #'nim-compile)
  ;; disable nimsuggest since using LSP
  ;; (remove-hook! 'nim-mode-hook #'+nim-init-nimsuggest-mode-h)
  )

(defvar nls/id 0)
(use-package! lsp-mode
  :init
  :config
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "~/bin/nimls")
  ;;                     :major-modes '(nim-mode)
  ;;                     ;; :priority -2
  ;;                     :priority 2
  ;;                     :server-id 'nimls))
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "nimlangserver")
  ;;                   :major-modes '(nim-mode)
  ;;                   ;; :priority -2
  ;;                   :priority 2
  ;;                   :server-id 'nimlangserver))
  )

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
            (call-process "nimpretty" nil nil nil
                          tmpfile
                          "--indent:" (number-to-string  nim-indent-offset)
                          "--maxLineLen:" nim-format-line-len)
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
