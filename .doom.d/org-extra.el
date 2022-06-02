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


(use-package! org-pretty-table
  :commands (org-pretty-table-mode global-org-pretty-table-mode))
