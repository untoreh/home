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

(if (featurep! :lang nim)
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

(defun nim-repl-toggle-debug () (error "Not implemented."))
