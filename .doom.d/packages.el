;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
                                        ;
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; misc
;; (package! aio)
(package! devdocs :recipe (:host github :repo "astoff/devdocs.el"))

;; codeium
(package! codeium :recipe (:host github :repo "Exafunction/codeium.el"))
;; gptel
(package! gptel)

;; HOTFIX
;; (unpin! compat evil vertico cape pdf-tools)
;; HOTFIX (corfu)
(unpin! evil-collection)
;; (after! popup
;;   (set-popup-rule! "^\\*mu4e-\\(main\\|headers\\)\\*" :ignore t))

;; completion
(package! corfu-candidate-overlay :recipe (:host github :repo "emacsmirror/corfu-candidate-overlay"))
(package! corfu-doc :recipe (:host github :repo "galeo/corfu-doc"))

;; (package! hydra-posframe :recipe
;;   (:host nil
;;    :repo "https://github.com/Ladicle/hydra-posframe"))
(package! aas :recipe (:host github :repo "ymarco/auto-activating-snippets"))

;; langs
(when (modulep! :tools org +poly)
  (package! poly-markdown
    :recipe
    (:host github :repo "polymode/poly-markdown"))
  (package! poly-org
    :recipe
    (:host github :repo "polymode/poly-org")))
(package! emacs-refactor :recipe (:host nil
                                  :repo "https://github.com/Wilfred/emacs-refactor"))
(package! treesit-auto :recipe (:host github :repo "renzmann/treesit-auto"))

;; nim
(package! nim-mode :pin "2cdbdf10d504d8ff4db7a655276e3c554043ac14")

;; julia
(if (modulep! :lang julia)
    (progn
      (unpin! (:lang julia))
      ;; lsp
      (if (modulep! :lang julia +lsp)
          (if (modulep! :tools lsp +eglot)
              (package! eglot-jl)
            (progn
              (package! lsp-julia
                :recipe (:host github :repo "gdkrmr/lsp-julia"))

              )))
      (package! julia-ts-mode)
      ;; standalone formatter which works as client/server
      ;; (if (modulep! :lang julia +format)
      ;;     (package! julia-formatter
      ;;       :recipe (:host nil
      ;;                :repo "https://codeberg.org/FelipeLema/julia-formatter.el"
      ;;                :files ("*.el" "*.jl" "*.toml"))))
      ;; snail provides repl/completions and other stuff
      (if (modulep! :lang julia +snail)
          (package! julia-snail))
      ))

;; shell
;; (package! powershell
;;   :ignore wslp)
(package! nushell-mode :recipe (:host github :repo "azzamsa/emacs-nushell"))
(package! eat)

;; jupyter
(package! jupyter)
(package! simple-httpd)
(package! websocket)
(package! zmq)

;; graphics
;; (package! nyan-mode)
(package! info-colors)
;; TODO: check back on newer emacs versions (>29.1)
(package! indent-bars :recipe (:host github :repo "jdtsmith/indent-bars"))
(when (modulep! :email mu4e)
  (package! mu4e-views)
  (package! mu4e-alert :disable t))


;; org
(package! ox-gfm)
(package! org-appear :recipe (:host github :repo "awth13/org-appear")
  :pin "8dd1e564153d8007ebc4bb4e14250bde84e26a34")
(package! org-transclusion)
(package! org-modern)

;; misc
(package! caddyfile-mode)
(package! vimrc-mode)
(package! systemd)
(when wslp
  (package! ahk-mode))

;; TODO: managing services with prodigy is nice especially on WSL
;; but I would prefer if the `init' would be a separate emacs instance with only
;; prodigy (and its deps) installed to which a client instance would connect to
;; interact with processes
;; (package! prodigy)

(package! keycast)
(package! gif-screencast)

;; ebooks
(package! restclient)
(package! calibredb :recipe (:host github :repo "chenyanming/calibredb.el" :branch "opds"))
(package! arxiv-mode :recipe (:host github :repo "fizban007/arxiv-mode"))
(package! nov)
