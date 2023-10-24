;;; ../../../var/home/fra/.doom.d/langs/python-bindings.el -*- lexical-binding: t; -*-

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

(map! :after python-repl
      (:prefix ("SPC l p" . "python")
       :desc "start python repl"
       :nev "r" (cmd! (python-repl-switch))
       :nev "." #'python-repl-cd
       :nev "d" #'python-repl-toggle-debug
       :nev "D" (cmd! (my/toggle-env-var "PYTHON_DEBUG" "debug"))
       :nev "v" #'python-repl-revise
       )
      (:map 'python-repl-mode-map
       "C-c C-p" nil
       "C-c C-v" nil
       "C-c ." #'python-repl-cd
       :desc nil
       "C-c C-." #'python-repl-cd))
