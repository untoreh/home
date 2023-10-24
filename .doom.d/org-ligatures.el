;;; ../../../var/home/fra/.doom.d/org-ligatures.el -*- lexical-binding: t; -*-

(appendq! +ligatures-extra-symbols
          (list :list_property "∷"
                :em_dash       "—"
                :ellipses      "…"
                :arrow_right   "→"
                :arrow_left    "←"
                :arrow_lr      "↔"
                :properties    "⚙"
                :end           "∎"
                :priority_a    #("⚑" 0 1 (face nerd-icons-red))
                :priority_b    #("⬆" 0 1 (face nerd-icons-orange))
                :priority_c    #("■" 0 1 (face nerd-icons-yellow))
                :priority_d    #("⬇" 0 1 (face nerd-icons-green))
                :priority_e    #("❓" 0 1 (face nerd-icons-blue))))

(defadvice! +org-init-appearance-h--no-ligatures-a ()
  :after #'+org-init-appearance-h
  (set-ligatures! 'org-mode nil)
  (set-ligatures! 'org-mode
    :list_property "::"
    :em_dash       "---"
    :ellipsis      "..."
    :arrow_right   "->"
    :arrow_left    "<-"
    :arrow_lr      "<->"
    :properties    ":PROPERTIES:"
    :end           ":END:"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"
    :priority_d    "[#D]"
    :priority_e    "[#E]"))
