(use-package! python
  :commands (python-mode)
  :config
  (load! "python-bindings")
  ;; make isort compatible with black
  (setq-default py-isort-options '("--profile" "black"))
  (add-hook 'python-mode-hook #'python-repl-mode)

  (setq
   ;; FIXME complains about the python interpreter not supporting completion...
   python-shell-completion-native-enable nil
   ;; python-shell-interpreter "python"
   ;; FIXME https://github.com/jorgenschaefer/elpy/issues/1744
   +python-jupyter-command '("jupyter" "console"))
  ;; use jupyter repl as default python repl
  (setf (alist-get 'python-mode +eval-repls)
        '(+python/open-jupyter-repl :persist t))

  (defun python-toggle-debug-envvar ()
    (interactive)
    (setenv "NIM_DEBUG"
            (if (equal "DEBUG" (getenv "PYTHON_DEBUG"))
                "" "DEBUG")))
  (after! python-repl
    (python-repl-set-terminal-backend 'vterm))
  ;; NOTE: pyvenv-activate needs the path the virtual env directory (usually .venv or .env)
  (after! pyvenv
    (add-hook! python-mode
      (require 'python-repl)
      (require 'f)
      (if (f-exists-p
           (my/concat-path (projectile-project-root) python-repl-venv-dir))
          (pyvenv-tracking-mode 1))))
  (set-popup-rules!
    '(("^\\*python\\*$" :height 25 :quit t :select nil)))
  ;; ;; livepy
  ;; (add-hook 'live-py-mode-hook
  ;;           (lambda ()
  ;;             (progn
  ;;               (setq-default live-py-version (executable-find "python"))
  ;;               (live-py-update-all))))
  )
