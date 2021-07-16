self: super:
# Packages exposed as part of nixuserPackages
let
  # nixl package
  nixgl = import (builtins.fetchTarball
    "https://github.com/guibou/nixGL/archive/master.tar.gz") { };
  nixexprs = builtins.fetchTarball {
    url = "https://nixos.org/channels/nixpkgs-unstable/nixexprs.tar.xz";
  };
  unstable = import nixexprs {
    inherit (self) config;
    overlays = [ ]; # no overlays inside overlay (infinite recursion)
  };
in {
  inherit (super)
  # shell
    starship eternal-terminal browsh zsh git-hub git-lfs pet tmux fzf fd bat
    ripgrep zoxide exa
    # utils
    wego translate-shell weechat krita calibre koreader
    # files and backups
    duplicacy borgbackup rclone syncthing
    # desktop
    mako wl-clipboard
    # theme
    noto-fonts noto-fonts-cjk noto-fonts-emoji noto-fonts-emoji-blob-bin
    noto-fonts-extra
    # input-fonts
    # system (don't use fish from nix because of locales)
    man less thefuck glibcLocales tini libqalculate dhcp bandwhich consul
    haproxy rsync sshpass pandoc zip docker xclip cachix

    # langs
    go-pup nixfmt golangci-lint shfmt libtidy nodejs shellcheck jq enchant2 ispell languagetool

    # runtimes
    python3

    emacsGcc # wait for mesa-d3d12
    mu isync gnupg pinentry emacs-all-the-icons-fonts;
  # dictionary
  inherit (self.aspellDicts) en;
  # dependencies
  inherit (self.weechatScripts) weechat-matrix;
  # gpu, waiting for nixpkgs integration
  # nixGLDefault
  inherit (nixgl) nixGLDefault;
  # wait for mesa-d3d12
  # inherit (unstable) mpv youtube-dl;
  inherit (unstable) youtube-dl nu;
  # python
  inherit (super.python38Packages) supervisor grip ansible;
}
