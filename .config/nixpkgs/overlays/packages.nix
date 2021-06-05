self: super:
# Packages exposed as part of nixuserPackages
let
  # nixl package
  nixgl = import (builtins.fetchTarball
    "https://github.com/guibou/nixGL/archive/master.tar.gz") { };
  nixexprs = builtins.fetchTarball {
    url = "https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz";
  };
  unstable = import nixexprs {
    inherit (self) config;
    overlays = [ ]; # no overlays inside overlay (infinite recursion)
  };
in {
  inherit (super)
    # shell
    starship eternal-terminal browsh git-hub pet tmux fzf fd bat ripgrep zoxide exa 
    # utils
    wego translate-shell weechat
    # files and backups
    duplicacy borgbackup rclone syncthing
    # desktop
    mako wl-clipboard
    # theme
    # input-fonts
    # system (don't use fish from nix because of locales)
    man less thefuck glibcLocales tini libqalculate dhcp bandwhich consul haproxy

    # langs
    go-pup nixfmt golangci-lint shfmt libtidy

    # runtimes
    python3

    # emacs
    # emacsPgtkGcc  # wait for mesa-d3d12
    mu isync gnupg pinentry emacs-all-the-icons-fonts neovim;
  # dependencies
  inherit (self.weechatScripts) weechat-matrix;
  # gpu, waiting for nixpkgs integration
  # nixGLDefault
  inherit (nixgl) nixGLDefault;
  # wait for mesa-d3d12
  # inherit (unstable) mpv youtube-dl;
  inherit (unstable) youtube-dl;
  # python
  inherit (super.python38Packages) supervisor grip ansible;
}
