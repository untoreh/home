;;; projects.el -*- lexical-binding: t; -*-

;; projectile
(setq projectile-ignored-projects '("~/" "/tmp" "~/tmp" "~/.local" "~/.cache" ".npm" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))
