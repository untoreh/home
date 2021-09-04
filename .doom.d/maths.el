;;; maths.el -*- lexical-binding: t; -*-

(map! (:map calc-mode-map
       :after calc
       :localleader
       :desc "Embedded calc (toggle)" "e" #'calc-embedded)
      (:map org-mode-map
       :after org
       :localleader
       :desc "Embedded calc (toggle)" "E" #'calc-embedded)
      (:map latex-mode-map
       :after latex
       :localleader
       :desc "Embedded calc (toggle)" "e" #'calc-embedded))
