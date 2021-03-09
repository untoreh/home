(setq common-layers
      '(
        html
        spell-checking
        theming
        ibuffer
        (ranger :variables
                ranger-dont-show-binary t)
        ;; auto-completion
        (auto-completion :variables
                         auto-completion-enable-snippets-in-popup t
                         auto-completion-enable-help-tooltip t
                         auto-completion-enable-sort-by-usage t)
        better-defaults
        ;; ivy
        (ivy :variables ivy-extra-directories nil
             ivy-re-builders-alist '(spacemacs/ivy--regex-plus)
             ivy-height 25)
        ;; colors
        (colors :variables colors-enable-nyan-cat-progress-bar t
                nyan-animate-nyancat nil
                nyan-wavy-trail t
                nyan-minimum-window-width 1024
                )
        )
      )
  (if (is-emacs-for-apps)
      (setq user-layers '(mu4e)
            mu4e-enable-async-operations t
            mu4e-enable-notifications t
            mu4e-enable-mode-line t
            mu4e-use-maildirs-extension t
            mu4e-index-cleanup nil
            mu4e-index-lazy-check t
            mu4e-installation-path "/usr/share/emacs/site-lisp")
    (setq user-layers
          '(ansible
            spacemacs-editing
            rust
            lua
            systemd
            csv
            ruby
            ;; ycmd
            emacs-lisp
            vimscript
            lsp
            dap
            (go :variables
                go-use-golangci-lint t
                go-format-before-save t
                godoc-at-point-function 'godoc-gogetdoc
                gofmt-command "goimports"
                go-backend 'lsp
                go-tab-width 4
                )
            geben
            sql
            nginx
            php
            (javascript :variables javascript-disable-tern-port-files nil)
            (python :variables python-backend 'lsp
                    python-lsp-server 'lsp-pyright
                    python-formatter 'black
                    python-format-on-save nil
                    python-enable-yapf-format-on-save nil
                    )
            ipython-notebook
            jupyter
            yaml
            (markdown :variables
                      markdown-live-preview-engine 'vmd
                      markdown-command "/usr/local/bin/pandoc")
            (docker :variables
                    docker-tramp-use-names t)
            ;; (wakatime :variables
            ;;           wakatime-api-key  "2148a59a-5931-4944-960a-23c3d28c88cb"
            ;;           wakatime-cli-path "/home/fra/.bin/wakatime")
            git
            github
            org
            deft
            speed-reading
            ;; semantic
            plantuml
            pandoc
            syntax-checking
            gtags
            shell-scripts
            (shell :variables
                   shell-default-height 30
                   shell-default-position 'bottom)
            (version-control :variables
                             projectile-enable-caching t
                             projectile-git-submodule-command "git submodule --quiet foreach 'echo $path' 2>/dev/null | tr '\\n' '\\0'"
                             )
            )
          )
    )

(setq user-layers (append user-layers common-layers))
(configuration-layer/declare-layers user-layers)
