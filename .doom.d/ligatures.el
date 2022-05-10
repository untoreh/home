;;; ../../../var/home/fra/.doom.d/ligatures.el -*- lexical-binding: t; -*-

(after! ligatures
  (defun ligatures--to-modes (modes)
    (mapc (lambda (mode)
            (print "setting ligatures for ")
            (print (concat (symbol-name mode) "-mode"))
            (set-ligatures! (concat mode "-mode")
              ;; Functional
              :lambda        "lambda"
              :def           "function"
              :composition   "composition"
              :map           "map/dictionary"
              ;; Types
              :null          "null"
              :true          "true"
              :false         "false"
              :int           "int"
              :float         "float"
              :str           "string"
              :bool          "boolean"
              :list          "list"
              ;; Flow
              :not           "not"
              :in            "in"
              :not-in        "not in"
              :and           "and"
              :or            "or"
              :for           "for"
              :some          "some"
              :return        "return"
              :yield         "yield"
              ;; Other
              :union         "Union"
              :intersect     "Intersect"
              :diff          "diff"
              :tuple         "Tuple"
              :pipe          "Pipe" ;; FIXME: find a non-private char
              :dot           "Dot")) modes))

  (ligatures--to-modes enabled-langs))
