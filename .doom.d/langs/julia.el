;;; .../../../var/home/fra/.doom.d/private/langs/julia.el -*- lexical-binding: t; -*-

;; TODO: add to julia-repl major mode hook, to set buffer name suffix, to avoid killing the vterm buffer when switchign projects
;;

(setq-hook! 'julia-mode-hook
  markdown-spaces-after-code-fence 0 ;; spaces break highlightjs lang inferring (and possibly other stuff)
  lsp-auto-guess-root nil
  orderless-component-separator "" ;; Splits on every char. Allows "AbS" to match "AbstractString"
  )
(after! julia-mode
  (set-popup-rule! "^\\*julia:" :height 25 :quit t :select nil)
  (when (modulep! :lang julia +lsp)
    (add-hook! julia-mode #'lsp))
  (add-hook! 'julia-mode-hook
    (setenv "JULIA_NUM_THREADS" (number-to-string (num-processors)))
    ;; https://github.com/doomemacs/doomemacs/commit/acae9f9acb328c46f71b4cc975abcdb95c09cee6
    ;; (setq-local lsp-enable-folding t
    ;;             lsp-folding-range-limit 100
    ;;             lsp-response-timeout 300)
    )
  ;; Ensure lsp is always active if lsp-mode is enabled
  (add-hook! '(doom-switch-buffer-hook doom-switch-window-hook)
    (when (and (member major-mode '(julia-ts-mode julia-mode))
               (eq lsp-mode t)
               (not lsp-buffer-uri)
               (doom-visible-buffer-p (current-buffer)))
      (lsp))))

;; julia-ts-mode
(use-package! julia-ts-mode
  :if (featurep! :lang julia)
  :mode "\\.jl$"
  :config
  (add-hook! (julia-mode julia-ts-mode) #'lsp)
  (add-to-list 'lsp-language-id-configuration '(julia-ts-mode . "julia"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-julia--rls-command)
                    :major-modes '(julia-mode ess-julia-mode julia-ts-mode)
                    :server-id 'julia-ls
                    :multi-root t)))
(use-package! lsp-julia
  :if (and (modulep! :lang julia +lsp)
	   (not (modulep! :tools lsp +eglot)))
  ;; must be set before lsp-mode is loaded
  :init
  (setq lsp-julia-response 360
	lsp-julia-timeout 360
	lsp-julia-package-dir nil)
  :config
  ;; for the --project flag to be buffer local
  ;; (make-variable-buffer-local 'lsp-julia-flags)
  (make-variable-buffer-local 'lsp-julia-default-depot)
  ;; override doom module preset
  (setq-default lsp-julia-default-environment
                (file-truename
                 (concat "~/.julia/environments/v"
                         (s-chomp (shell-command-to-string "julia --version | grep -oE '[0-9]\.[0-9]'"))))
                lsp-julia-default-depot (shell-command-to-string "julia -e \"print(join(DEPOT_PATH,\\\":\\\"))\"")
                lsp-julia-lint-missingrefs "all") ;; julia LS can't find symbols from include modules
  ;; These are basically all useless
  ;; NOTE: This does not seem to make any difference
  ;; (add-hook! julia-mode
  ;;   (let ((root (projectile-project-root)))
  ;;     ;; (setq-local lsp-julia-default-environment root)
  ;;     (my/concatq! lsp-julia-default-depot "\:" root)))
  ;; NOTE: The LS can't figure out recursive dependencies so this is not really helpful, but avoids
  ;; starting multiple LS instances
  (defun julia-project-root ()
    (or (getenv "JULIA_PROJECT") (projectile-project-root) ""))
  (after! projectile
    (defadvice! projectile-julia-project-root nil :override
      #'lsp-julia--get-root
      (concat "\"" (julia-project-root) "\"")))
  ;; NOTE: Precompilation causes runtime errors of methods not found...
  ;; (let* ((sysimage (file-truename "~/.julia/lsp/languageserver.so"))
  ;;        (flag (concat "--sysimage=" sysimage)))
  ;;   (when (or (file-exists-p sysimage)
  ;;             (when (yes-or-no-p "Compile julia system image with language server?")
  ;;               (async-shell-command "~/.julia/lsp/compile.sh")
  ;;               (message "Started Julia process to compile LanguageServer system image. This may take a while.")))
  ;;     (pushnew! lsp-julia-flags flag)
  ;;     (setq-default lsp-julia-flags lsp-julia-flags)))
  ;; (setq lsp-julia-command "~/.julia/sysimage/compiled/bin/julia")
  )

;; Not used: julia-ob, julia-snail, julia-proto
(load! "julia-repl")
(load! "julia-franklin")

;; julia projects file
(after! projectile
  ;; (appendq! projectile-project-root-files '("Project.toml" "JuliaProject.toml"))
  (setq-hook! 'julia-mode-hook projectile-project-test-cmd
              "julia --startup-file=no --project=test/ test/runtests.jl --test-args '' ")
  )
