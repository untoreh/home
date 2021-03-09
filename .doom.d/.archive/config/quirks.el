;; quirks
;; disable semantic top header sticky function
;; (add-hook 'semantic-mode-hook 'spacemacs/toggle-semantic-stickyfunc-off)

;; elisp company-capf semantic slowdown
;; (eval-after-load 'semantic (lambda()
;;                              (if (boundp 'semantic-workaround)
;;                                  'nil
;;                                (setq semantic-workaround t)
;;                                (add-hook 'semantic-mode-hook
;;                                          (lambda ()
;;                                            (dolist (x (default-value 'completion-at-point-functions))
;;                                              (when (string-prefix-p "semantic-" (symbol-name x))
;;                                                (remove-hook 'completion-at-point-functions x))))))))

;; workarounds
;; disable purpose-mode because switch-to-buffer has broken args
;; (purpose-mode 0)
;; quirks
;; (require 'semantic/db-file)
