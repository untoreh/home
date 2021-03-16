self: super:
# Packages exposed as part of nixuserPackages
let
  # nixl package
  nixgl = import (builtins.fetchTarball
    "https://github.com/guibou/nixGL/archive/master.tar.gz") { };
in {
  inherit (super)
    browsh kitty git-hub pet tmux fzf fd bat ripgrep zoxide exa bandwhich wego
    duplicacy borgbackup translate-shell rclone consul haproxy
    # sway
    mako
    # theme
    # input-fonts

    # langs
    go-pup nixfmt golangci-lint shfmt

    # emacs
    emacsGcc mu emacs-all-the-icons-fonts;
  # gpu, waiting for nixpkgs integration
  # nixGLDefault
  inherit (nixgl) nixGLDefault;
  # backup
  # borg;
  # python
  inherit (super.python38Packages) howdoi;
  # inherit (super.nixgl) nixGlIntel;
}
