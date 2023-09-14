;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

;; compile lsp-mode with plists support
(setenv "LSP_USE_PLISTS" "1")
(setq lsp-use-plists t)

(doom! :input
       ;;
       :completion
       ;; don't use tng since we prefer TAB+SPC and ENTER for inserting completions
       ;; and don't use childframes because are slow
       ;; (company)           ; the ultimate code completion backend
       (vertico +icons)
       ;; (ivy +prescient +icons +childframe)               ; a search engine for love and life

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       (emoji +ascii +unicode +github)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       ;; indent-guides     ; highlighted indent columns
       (ligatures +fira)         ; ligatures and symbols to make your code pretty again
       ;; minimap
       (modeline)          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink cursor line after big motions
       ophints           ; highlight the region an operation acts on
       ;; use `popper' instead of :ui popup
       (popup +defaults +all)   ; tame sudden yet inevitable temporary windows
       ;; tabs
       (treemacs +lsp)          ; a project drawer, like neotree but cooler
       unicode           ; NOTE: we use "Julia Mono" this module overrides doom-unicode-font
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       (window-select +switch-window +numbers)     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       format  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;; lispy             ; vim for lisp, for people who don't like vim
       multiple-cursors
                                        ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +dirvish +icons)             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer         ; interactive buffer management
       (undo +tree)              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       vterm             ; the best terminal emulation in Emacs

       :checkers
       (syntax +flymake +childframe)              ; tasing you for every semicolon you forget
       ;; (spell +flyspell +aspell +everywhere)             ; tasing you for misspelling mispelling
       grammar           ; tasing grammar mistake every you make

       :tools
       ansible
       debugger          ; FIXME stepping through code, to help you add bugs
       direnv
       (docker +lsp)
       editorconfig      ; let someone else argue about tabs vs spaces
       ;; ein               ; tame jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       ;; BROKEN gist              ; interacting with github gists
       ;; use `devdocs' package instead of `+docsets'
       (lookup +dictionary +offline)              ; navigate your code and its documentation
       (lsp)
       magit             ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       rgb               ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       ;; (:if IS-MAC macos)  ; improve compatibility with macOS
       (tty +osc)               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;;cc                ; C/C++/Obj-C madness
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       (csharp +dotnet +lsp +tree-sitter)            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;; ess               ; emacs speaks statistics
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;;(go +lsp)         ; the hipster dialect
       ;;(haskell +dante)  ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       (json +lsp)              ; At least it ain't XML
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       (julia +lsp)             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;lean
       ;;factor
       ;;ledger            ; an accounting system in Emacs
       (lua +lsp) ; One-based indices? one-based indices
       (markdown +grip)          ; writing docs for people to ignore
       ;; FIXME: nim module is useless because nimsuggest doesn't work, use LSP with `nimlangserver'
       ;; (nim +lsp)               ; python + lisp at the speed of c
       nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org +dragndrop +pandoc +poly +gnuplot +noter) ; organize your plain life in plain text
       ;; php               ; perl's insecure younger brother
       plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python +lsp +pyright +pyimport +py-isort +conda)            ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       racket            ; a DSL for DSLs
       raku              ; the artist formerly known as perl6
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp)              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;scheme            ; a fully conniving family of lisps
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       web               ; the tubes
       (yaml +lsp)              ; JSON, but readable

       :email
       ;; don't use +gmail since it overrides mu4e del hotkeys
       (mu4e +org)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       everywhere
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings +smartparens))

(when (fboundp 'native-compile-async)
  (setq
   comp-async-report-warnings-errors nil
   native-comp-deferred-compilation t
   native-comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))

