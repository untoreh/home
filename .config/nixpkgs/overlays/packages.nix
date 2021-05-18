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
    starship eternal-terminal browsh git-hub pet tmux fzf fd bat ripgrep zoxide
    exa bandwhich wego duplicacy borgbackup translate-shell rclone consul
    weechat haproxy
    # kitty wait for d3d12
    # sway
    mako wl-clipboard
    # theme
    # input-fonts
    # docs, don't use fish from nix because of locales 
    man less thefuck glibcLocales tini

    # langs
    go-pup nixfmt golangci-lint shfmt

    # runtimes
    python3

    # emacs
    # emacsPgtkGcc  # wait for mesa-d3d12
    mu isync gnupg pinentry emacs-all-the-icons-fonts neovim;
  inherit (self.weechatScripts) weechat-matrix;
  # gpu, waiting for nixpkgs integration
  # nixGLDefault
  inherit (nixgl) nixGLDefault;
  # wait for mesa-d3d12
  # inherit (unstable) mpv youtube-dl;
  inherit (unstable) youtube-dl;
  # backup
  # borg;
  # python
  inherit (super.python38Packages) supervisor grip;
}
