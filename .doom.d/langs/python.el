;; ;; livepy
;; (add-hook 'live-py-mode-hook
;;           (lambda ()
;;             (progn
;;               (setq-default live-py-version (executable-find "python"))
;;               (live-py-update-all))))

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (setq indent-tabs-mode nil)
;;             (setq tab-width 4)
;;             (setq evil-shift-width 4)))

;; (use-package! ein
;;   :config
;;   (setq-default
;;    ;; allow whole cells undo
;;    ein:worksheet-enable-undo nil
;;    ;; multi major mode support
;;    ein:polymode t)
;;   (custom-set-faces
;;    '(ein:cell-input-area ((t (:background "#262833"))))))

(after! python-mode
  :config
  ;; make isort compatible with black
  (setq-default py-isort-options '("--profile" "black"))
  (add-to-list '+format-on-save-enabled-modes 'python-mode t)
  ;; use jupyter repl as default python repl
  (setf (alist-get 'python-mode +eval-repls)
        '(+python/open-jupyter-repl :persist t)))
