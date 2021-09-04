;;; langs/spell.el -*- lexical-binding: t; -*-

(require 'functions)

(setq
 asp
 ispell-dictionary "en-variant_2"
 ispell-aspell-data-dir (concat (file-name-as-directory
                                 (getenv "HOME"))
                                ".nix-profile/lib/aspell/")

 ispell-personal-dictionary
 (expand-file-name ".ispell_personal" doom-private-dir))

;; ensure symlink for data dir for enchant
(my/ensure-symlink "~/.nix-profile/lib/aspell" "~/.config/enchant/aspell")
