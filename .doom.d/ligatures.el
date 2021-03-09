;;; ../../../var/home/fra/.doom.d/ligatures.el -*- lexical-binding: t; -*-

(defun ligatures--to-modes (modes)
  (mapc (lambda (mode)
          (print "setting ligatures for ")
        (print (concat (symbol-name mode) "-mode"))
         (set-ligatures! (concat mode "-mode")
           ;; Functional
           :lambda        "lambda keyword"
           :def           "function keyword"
           :composition   "composition"
           :map           "map/dictionary keyword"
           ;; Types
           :null          "null type"
           :true          "true keyword"
           :false         "false keyword"
           :int           "int keyword"
           :float         "float keyword"
           :str           "string keyword"
           :bool          "boolean keywork"
           :list          "list keyword"
           ;; Flow
           :not           "not operator"
           :in            "in operator"
           :not-in        "not in operator"
           :and           "and keyword"
           :or            "or keyword"
           :for           "for keyword"
           :some          "some keyword"
           :return        "return"
           :yield         "yeild"
           ;; Other
           :union         "Union keyword"
           :intersect     "Intersect keyword"
           :diff          "diff keyword"
           :tuple         "Tuple Keyword "
           :pipe          "Pipe Keyword" ;; FIXME: find a non-private char
           :dot           "Dot operator")) modes))

(after! ligatures
  (ligatures--to-modes enabled-langs))
