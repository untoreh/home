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
(package! mu4e-alert)
;(package! gcmh :disable t)
(package! aio)
(package! devdocs :recipe (:host github :repo "astoff/devdocs.el"))

;; langs
(package! poly-markdown :recipe
  (:host github :repo "polymode/poly-markdown"))
(package! poly-org :recipe
  (:host github :repo "polymode/poly-org"))
(package! tree-sitter)
(package! tree-sitter-langs)
;; julia
(if (featurep! :lang julia)
    (progn
      (unpin! (:lang julia))
      ;; lsp
      (if (featurep! :lang julia +lsp)
          (if (featurep! :tools lsp +eglot)
              (package! eglot-jl)
            (package! lsp-julia
              :recipe (:host github :repo "non-jedi/lsp-julia"))))
      ;; standalone formatter which works as client/server
      (if (featurep! :lang julia +format)
          (package! julia-formatter
            :recipe (:host nil
                     :repo "https://codeberg.org/FelipeLema/julia-formatter.el"
                     ;; :repo "https://github.com/ki-chi/julia-formatter"
                     :files ("*.el" "*.jl" "*.toml"))))
      ;; snail provides repl/completions and other stuff
      (if (featurep! :lang julia +snail)
          (package! julia-snail))
      ;; a featureful ob implementation for julia
      ;(if (featurep! :lang julia +ob)
      ;    (package! ob-julia :shadow 'ob-julia
      ;      :recipe (:host nil
      ;               :repo "https://git.nixo.xyz/nixo/ob-julia.git"
      ;               :files ("*.jl" "*.el"))))
      ))
;; shell
(package! fish-mode)
(package! powershell)

;; jupyter
(package! jupyter)
(package! simple-httpd)
(package! websocket)
(package! zmq)

;; graphics
(package! sweet-theme)
(package! parrot)
(package! info-colors)
;; (package! nyan-mode)

;; org
(package! valign)
(package! org-pretty-tags)
(package! ox-gfm)
(package! org-ref)
(package! org-tanglesync :recipe (:host nil
                                  :repo "https://gitlab.com/mtekman/org-tanglesync.el"
                                  :files ("*.el")))

;; ;; completion
(when (featurep! :completion company)
  (when (or (featurep! :completion company +tooltips)
            (featurep! :completion company +childframe))
    (package! company-quickhelp)))

;; (package! corfu)
(package! emacs-refactor :recipe ( :host nil
                                   :repo "https://github.com/Wilfred/emacs-refactor"))
;; which-key-posframe is VERY slow
;; (package! which-key-posframe)
(package! hydra-posframe :recipe ( :host nil
                                   :repo "https://github.com/Ladicle/hydra-posframe"))
;; chat
(package! weechat :recipe (:host github
                           :repo "bqv/weechat.el"))
;; auto hot key mode
;; (package! ahk-mode :recipe (:host nil
;;                             :repo "https://github.com/untoreh/ahk-mode"))
(package! ahk-mode)
(package! vimrc-mode)
(package! systemd)

;; we use weechat instead
;; (package! matrix-client
;;   :recipe (:host nil
;;    :repo "https://github.com/alphapapa/matrix-client.el"
;;    :files ("*.el" "logo.png" "matrix-client-standalone.el.sh")
;;    :post-build ((require 'f)
;;                 (let* ((script-name "/matrix-client-standalone.el.sh")
;;                        (script-path (concat (f-dirname (locate-library "matrix-client")) script-name))
;;                        (bindir (concat user-emacs-directory "/.local/bin"))
;;                        (link-path (concat bindir "matrix")))
;;                   (make-directory bindir t)
;;                   (delete-file link-path nil)
;;                   (make-symbolic-link script-path link-path t)))))

(package! gif-screencast)
(package! vertico-posframe)
;; VLF
(package! vlf
  :recipe (:host github :repo "m00natic/vlfi" :files ("*.el"))
  :disable t)

(package! rotate)
(when (featurep! :email mu4e)
  (package! mu4e-views))

;; TODO: managing services with prodigy is nice especially on WSL
;; but I would prefer if the `init' would be a separate emacs instance with only
;; prodigy (and its deps) installed to which a client instance would connect to
;; interact with processes
;; (package! prodigy)

;; adds colors to info docs buffers :pin "47ee73cc19b1049eef32c9f3e264ea7ef2aaf8a5"
(package! info-colors)

;; shows keychords in the modeline :pin "04ba7519f34421c235bac458f0192c130f732f12"
(package! keycast)

;; pcache needed by gcbal
(package! pcache)

;; ebooks
(package! restclient)
(package! calibredb :recipe (:host github :repo "chenyanming/calibredb.el" :branch "opds"))
(package! arxiv-mode :recipe (:host github :repo "fizban007/arxiv-mode"))
(package! nov)

;; FIXME: vterm workaround
(package! vterm :recipe
  (:host github
   :repo "blahgeek/emacs-libvterm"
   :branch "fix-visibility"))
