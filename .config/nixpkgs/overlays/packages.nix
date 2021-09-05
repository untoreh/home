self: super:
# Packages exposed as part of nixuserPackages
let
  # nixl package
  nixgl = import (builtins.fetchTarball
    "https://github.com/guibou/nixGL/archive/main.tar.gz") { };
in {
  inherit (super)
  # shell
    eternal-terminal browsh zsh git-hub git-lfs delta pet tmux fzf fd bat
    ripgrep zoxide exa nushell starship direnv
    # utils
    wego translate-shell weechat krita calibre koreader youtube-dl
    # files and backups
    duplicacy borgbackup rclone syncthing
    # desktop
    mako scrot pywal
    # reduntant
    # xclip wl-clipboard
    # theme
    noto-fonts noto-fonts-cjk noto-fonts-emoji noto-fonts-emoji-blob-bin font-awesome
    noto-fonts-extra sweet emojione fantasque-sans-mono julia-mono fira fira-code fira-code-symbols hack-font ia-writer-duospace
    powerline-fonts material-design-icons barlow
    # input-fonts
    # system (don't use fish from nix because of locales)
    man less thefuck glibcLocales tini libqalculate dhcp bandwhich consul
    haproxy rsync sshpass pandoc zip docker cachix

    # langs
    go-pup nixfmt golangci-lint shfmt libtidy nodejs shellcheck jq enchant2 ispell languagetool
    chrpath

    # runtimes
    # python3

    # emacsPgtkGcc # wait for mesa-d3d12
    mu isync gnupg pinentry emacs-all-the-icons-fonts;
  # more fonts
  inherit (self.vimPlugins) vim-devicons;
  # emacs with cachix support
  # sweet theme dep
  inherit (self.gnome3) adwaita-icon-theme;
  # dictionary
  inherit (self.aspellDicts) en en-computers en-science it;
  # dependencies
  inherit (self.weechatScripts) weechat-matrix;
  # gpu, waiting for nixpkgs integration
  # nixGLDefault
  inherit (nixgl.auto) nixGLDefault;
  # wait for mesa-d3d12
  # python
  inherit (super.python38Packages) supervisor grip ansible pip black flake8;
  # pandas isort setuptools timeago cython
  # deps for thefuck
  # colorama decorator psutil pyte;
}
