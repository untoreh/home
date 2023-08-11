;;; completion/bindings.el -*- lexical-binding: t; -*-

(map! :map corfu-map
        :i "TAB" #'corfu-complete
        :i [tab] #'corfu-complete
        :i "C-n" #'corfu-next
        :i "C-j" #'corfu-insert
        :i "C-SPC" #'corfu-reset
        :i "S-TAB" #'corfu-previous
        :i "C-p" #'corfu-previous
        :i [?\r] #'newline
        :i [backtab] #'corfu-previous)
(map! :after (corfu cape)
      (:desc "Symbol" :i "C-SPC l" (my/capf-cmd! cape-symbol-scapf))
      (:desc "Word" :i "C-SPC w" (my/capf-cmd! cape-word-scapf))
      (:desc "Expand" :i "C-SPC d" (my/capf-cmd! cape-expand-scapf))
      (:desc "File" :i "C-SPC f" (my/capf-cmd! cape-file-scapf))
      (:desc "Code" :i "C-SPC c" (my/capf-cmd! cape-char-scapf))
      (:desc "T9" :i "C-SPC SPC" (my/capf-cmd! cape-default-scapf))
      (:desc "Snippet" :i "C-SPC s" (my/capf-cmd! (cape-company-to-capf #'company-yasnippet)))
      ;(:mode lsp-mode
             ;(:desc "lsp" :i "C-SPC SPC" (my/capf-cmd! cape-t9-lsp-scapf))
             ;)
	     )
