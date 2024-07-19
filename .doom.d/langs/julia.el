;;; .../../../var/home/fra/.doom.d/private/langs/julia.el -*- lexical-binding: t; -*-

;; TODO: add to julia-repl major mode hook, to set buffer name suffix, to avoid killing the vterm buffer when switchign projects
;;

(setq-hook! 'julia-mode-hook
  markdown-spaces-after-code-fence 0 ;; spaces break highlightjs lang inferring (and possibly other stuff)
  lsp-auto-guess-root nil
  ;; NOTE: causes high load (would require async json emacs fork)
  ;; orderless-component-separator "" ;; Splits on every char. Allows "AbS" to match "AbstractString"
  )

(use-package! julia-mode
  :init
  (when (modulep! :lang julia +lsp)
    (add-hook! julia-mode #'lsp))
  (add-hook! 'julia-mode-hook
    (setenv "JULIA_NUM_THREADS" (number-to-string (num-processors)))
    ;; https://github.com/doomemacs/doomemacs/commit/acae9f9acb328c46f71b4cc975abcdb95c09cee6
    )
  (setq-hook! julia-mode
    lsp-enable-folding t
    lsp-folding-range-limit 100)
  :config
  ;; Ensure lsp is always active if lsp-mode is enabled
  (add-hook! '(doom-switch-buffer-hook doom-switch-window-hook)
    (when (and (member major-mode '(julia-ts-mode julia-mode))
               (eq lsp-mode t)
               (not lsp-buffer-uri)
               (doom-visible-buffer-p (current-buffer)))
      (lsp)))
  ;; Not used: julia-ob, julia-snail, julia-proto
  (load! "julia-repl")
  (load! "julia-franklin")
  ;; julia projects file
  (defun julia-project-root ()
    (or (getenv "JULIA_PROJECT") (projectile-project-root) ""))
  (after! projectile
    ;; (appendq! projectile-project-root-files '("Project.toml" "JuliaProject.toml"))
    (setq-hook! 'julia-mode-hook projectile-project-test-cmd
                "julia --startup-file=no --project=test/ test/runtests.jl --test-args '' ")
    ))

(use-package! lsp-julia
  :if (and (modulep! :lang julia +lsp)
	   (not (modulep! :tools lsp +eglot)))
  ;; must be set before lsp-mode is loaded
  :init
  (setq
   ;; lsp-julia-response 360
   ;; lsp-julia-timeout 360
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
  ;; NOTE: The LS can't figure out recursive dependencies so this is not really helpful, but avoids
  ;; starting multiple LS instances
  (after! projectile
    (defadvice! projectile-julia-project-root nil :override
      #'lsp-julia--get-root
      (concat "\"" (julia-project-root) "\"")))
  )
