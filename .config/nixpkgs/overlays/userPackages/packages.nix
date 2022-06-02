self: super:
# Packages exposed as part of nixuserPackages
let
  # nixl package
  nixgl = import (builtins.fetchTarball
    "https://github.com/guibou/nixGL/archive/22f96c15988d51f97110e3e79dbbee02f2a47273.tar.gz") { };
  unstable = import <nixpkgs-unstable> {
    inherit (self) config;
    #overlays = [ ]; # no overlays inside overlay (infinite recursion)
  };
in {
  inherit (super)
  # shell
    eternal-terminal browsh zsh git-hub git-lfs delta pet tmux fzf fd
    ripgrep zoxide exa nushell starship direnv bat pastel
    # utils
    wego translate-shell weechat krita calibre koreader youtube-dl djvulibre unoconv wordnet
    # files and backups
    duplicacy borgbackup rclone syncthing inotify-tools
    # desktop
    mako scrot pywal
    # reduntant
    # xclip wl-clipboard
    # theme
    noto-fonts noto-fonts-cjk noto-fonts-emoji noto-fonts-emoji-blob-bin font-awesome
    noto-fonts-extra sweet emojione fantasque-sans-mono julia-mono fira fira-code fira-code-symbols hack-font ia-writer-duospace
    powerline-fonts material-design-icons barlow
    # system (don't use fish from nix because of locales)
    man less thefuck glibcLocales tini libqalculate dhcp bandwhich consul
    haproxy rsync sshpass pandoc zip docker cachix msmtp catatonit

    # langs
    go-pup nixfmt golangci-lint shfmt libtidy nodejs shellcheck jq enchant2 ispell languagetool
    chrpath pipenv jre;
  inherit (unstable) mu isync goimapnotify gnupg keychain pinentry emacs-all-the-icons-fonts gost;
  # more fonts
  inherit (self.vimPlugins) vim-devicons;
  # sweet theme dep
  inherit (self.gnome) adwaita-icon-theme;
  # dictionary
  inherit (self.aspellDicts) en en-computers en-science it;
  # dependencies
  inherit (self.weechatScripts) weechat-matrix;
  # gpu, waiting for nixpkgs integration
  # nixGLDefault, disabled because of "unsupported-package" issue
  #inherit (nixgl.auto) nixGLDefault;
  inherit (nixgl) nixVulkanIntel;
  # wait for mesa-d3d12
  # python
  inherit (super.python39Packages) supervisor grip cffi ansible black flake8 ptpython;
  # pandas isort setuptools timeago cython
  # deps for thefuck
  # colorama decorator psutil pyte;
}
