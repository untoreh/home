;;; langs/spell.el -*- lexical-binding: t; -*-

(require 'functions)

;; NOTE: aspell (for installing dictionaries is installed from distro (not nix-env)
;; dictionaries are installed from nix-env and specified with `local-data-dir'
;; because spell-fu doesn't allow specifying `data-dir' so it always look for dicts
;; in system directories

(setq ispell-enchant-aspell-dir
      "~/.config/enchant/aspell")

(setq
 ;; spell-fu has troubles highlighting all the words
 ;; currently using flyspell, doesn't seem that slow, gccemacs?
 ispell-dictionary "en-wo_accents"
 ispell-aspell-data-dir "~/.nix-profile/lib/aspell"
 flyspell-delay 1)

;; ensure symlink for data dir for enchant
(my/ensure-symlink ispell-aspell-data-dir
                   ispell-enchant-aspell-dir)

(setq
 ispell-personal-dictionary
 (expand-file-name ".ispell_personal" doom-private-dir))

;; prefer ripgrep over grep for ispell lookup
(when-let ((rg (executable-find "rg")))
  (setq ispell-grep-command rg
        ispell-grep-options "-i"))

(after! company-ispell
  (let ((words-dict (my/concat-path doom-cache-dir "spell" "aspell-dict.txt")))
    (when (not (file-exists-p words-dict))
      ;; http://app.aspell.net/create?max_size=80&spelling=US&max_variant=2&diacritic=strip&special=hacker&special=roman-numerals&download=wordlist&encoding=utf-8&format=inline
      (warn "words dict file not found, download a copy to %s" words-dict))
    (setq ispell-alternate-dictionary words-dict)))

(after! ispell
  ;; check version again because something is setting aspell instead of 'enchant
  (ispell-check-version))
