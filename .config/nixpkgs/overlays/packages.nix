self: super:
# Packages exposed as part of nixuserPackages
{
  inherit (super)
    browsh kitty git-hub pet tmux fzf fd bat ripgrep zoxide exa bandwhich wego
    duplicacy translate-shell rclone consul haproxy
    # gpu, waiting for nixpkgs integration
    # nixGLDefault

    # sway
    mako
    # theme
    # input-fonts

    # langs
    go-pup nixfmt golangci-lint shfmt

    # emacs
    emacsGcc mu emacs-all-the-icons-fonts
    # …
  ;
  # python
  inherit (super.python38Packages) howdoi;
  # inherit (super.nixgl) nixGlIntel;
}
