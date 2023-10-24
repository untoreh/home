;;; org-extra.el -*- lexical-binding: t; -*-

(use-package! org-ol-tree
  :commands org-ol-tree
  :config
  (defadvice! org-ol-tree-system--graphical-frame-p--pgtk ()
    :override #'org-ol-tree-system--graphical-frame-p
    (memq window-system '(pgtk x w32 ns))))

(map! :map org-mode-map
      :after org
      :localleader
      :desc "Outline" "O" #'org-ol-tree)

(use-package! org-transclusion
  :commands org-transclusion-mode
  :init
  (map! :after org :map org-mode-map
        "<f12>" #'org-transclusion-mode))

(if (modulep! :lang org +jupyter)
    (after! ob-jupyter
      (load! "jupyter")
      (require 'jupyter-org-client)
      (jupyter-org-interaction-mode 1)
      (jupyter-repl-interaction-mode)))

(use-package! ox-gfm :after ox)

(use-package! org-modern
  :after-call org-mode
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  (global-org-modern-mode))
