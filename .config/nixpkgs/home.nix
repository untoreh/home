{ config, pkgs, ... }:

# nixpkgs.config.allowUnfree = true;

let
  user = "fra";
  home_dir = "/home/" + user;
  rice = home_dir + "/.rice";
in rec {
  # c = x: rice + "/.config/${x}";
  # s = x: rice + "/secrets/${x}";
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = user;
  home.homeDirectory = home_dir;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";

  # shell
  # xdg.configFile."fish/".source = rice + "/.config/fish/";
  # home.file.".config/fish".source = toString rice + "/.config/fish";
  # home.file.".tmux.conf".source = rice + "/dots/.tmux.conf";
  # home.file.".inputrc".source = rice + "/dots/.inputrc";
  # home.file.".bashrc".source = rice + "/dots/.bashrc";
  # home.file.".bash_history".source = rice + "/secrets/.bash_history";
  # home.file.".profile".source = rice + "/dots/.profile";
  # home.file.".functions".source = rice + "/dots/.functions";
  # home.file.".emacs-profiles.el".source = rice + "/dots/.emacs-profiles.el";
  # home.file.".doom.d/".source = rice + "/dots/.doom.d";
  # # home.file.".emac
  # # s.doom.d".source = home_dir + "/.emacs.d";
  # home.file.".docker/".source = rice + "/secrets/.docker";

  # # nix
  # home.file.".nix-channels/".source = rice + "/dots/.nix-channels";
  # xdg.configFile."nix/".source = rice + "/.config/nix/";

  # # de/theme
  # home.file.".gtkrc-2.0".source = rice + "/dots/.gtkrc-2.0";
  # home.file.".wegorc".source = rice + "/dots/.wegorc";
  # home.file.".Xdefaults".source = rice + "/dots/.Xdefaults";
  # home.file.".Xresources".source = rice + "/dots/.Xresources";
  # home.file.".Xsettingsd".source = rice + "/dots/.xsettingsd";
  # home.file.".nx/config/".source = rice + "/secrets/.nx/config";
  # home.file.".kde/".source = rice + "/dots/.kde";
  # xdg.configFile."Qlipper/".source = rice + "/.config/Qlipper";
  # xdg.configFile."gtk-3.0/".source = rice + "/.config/gtk-3.0/";
  # xdg.configFile."gtkrc".source = rice + "/.config/gtkrc";
  # xdg.configFile."gtkrc-2.0".source = rice + "/.config/gtkrc-2.0";
  # xdg.configFile."jellyfin-mpv-shim/".source = rice
  #   + "/.config/jellyfin-mpv-shim/";
  # xdg.configFile."Kvantum/".source = rice + "/.config/Kvantum/";
  # xdg.configFile."xournalpp/".source = rice + "/.config/xournalpp/";
  # xdg.configFile."Debauchee/".source = rice + "/.config/Debauchee/";
  # xdg.configFile."plasma-workspace/".source = rice
  #   + "/.config/plasma-workspace/";
  # xdg.configFile."plasmanotifyrc".source = rice + "/.config/plasmanotifyrc";
  # xdg.configFile."plasmarc".source = rice + "/.config/plasmarc";
  # xdg.configFile."plasmashellrc".source = rice + "/.config/plasmashellrc";
  # xdg.configFile."plasma-localerc".source = rice + "/.config/plasma-localerc";
  # xdg.configFile."plasma-org.kde.plasma.desktop-appletsrc".source = rice
  #   + "/.config/plasma-org.kde.plasma.desktop-appletsrc";
  # xdg.configFile."kde.org/".source = rice + "/.config/kde.org/";
  # xdg.configFile."kdebugrc".source = rice + "/.config/kdebugrc";
  # xdg.configFile."kded5rc".source = rice + "/.config/kded5rc";
  # xdg.configFile."kded_device_automounterrc".source = rice
  #   + "/.config/kded_device_automounterrc";
  # xdg.configFile."kdeglobals".source = rice + "/.config/kdeglobals";
  # xdg.configFile."xsettingsd/".source = rice + "/.config/xsettingsd/";

  # #media
  # # xdg.configFile."trakt-scrobbler/" = c "trakt-scrobbler/";

  # # links misc
  # home.file.".bin/".source = rice + "/bin";
  # home.file."scripts/".source = rice + "/scripts";
  # home.file.".cluster/".source = rice + "/secrets/cluster";
  # home.file.".aws/".source = rice + "/secrets/aws";
  # home.file.".deft/".source = rice + "/secrets/.deft";
  # home.file.".wallets/".source = rice + "/secrets/.wallets";
  # home.file.".remmina/".source = rice + "/secrets/.remmina";
  # home.file.".travis/".source = rice + "/secrets/.travis";
  # home.file.".vim/init.vim".source = rice + "/dots/.vim/init.vim";
  # home.file.".vim/init.vim".onChange =
  #   "ln -sr $home_dir/.vimrc $home_dir/.vim/init.vim";
  # home.file.".vimrc".source = home_dir + "/.vim/init.vim";
  # home.file.".ssh/".source = rice + "/secrets/.ssh";
  # home.file.".git-credentials".source = rice + "/secrets/.git-credentials";
  # home.file.".gitconfig".source = rice + "/secrets/.gitconfig";
  # home.file.".authinfo".source = rice + "/secrets/.authinfo";
  # home.file.".authinfo.gpg".source = rice + "/secrets/.authinfo.gpg";
  # home.file.".netrc".source = rice + "/secrets/.netrc";
  # home.file.".mbsyncrc".source = rice + "/secrets/.mbsyncrc";
  # home.file.".rclone.conf".source = rice + "/secrets/.rclone.conf";
  # home.file.".gnupg/".source = rice + "/secrets/.gnupg";
  # home.file.".ngrok2/".source = rice + "/secrets/.ngrok2";
  # home.file.".vagrant.d/".source = rice + "/secrets/.vagrant.d";
  # home.file.".vcpkg/".source = rice + "/secrets/.vcpkg";

  # # links langs
  # home.file.".viminfo".source = rice + "/secrets/.viminfo";
  # home.file.".ansible-console_history".source = rice
  #   + "/secrets/.ansible-console_history";
  # home.file.".node_repl_history".source = rice + "/secrets/.node_repl_history";
  # home.file.".npmrc".source = rice + "/dots/.npmrc";
  # home.file.".pylintrc".source = rice + "/dots/.pylintrc";
  # home.file.".python_history".source = rice + "/secrets/.python_history";
  # home.file.".ipython/".source = rice + "/secrets/.ipython";
  # home.file.".jupyter/".source = rice + "/secrets/.jupyter";
  # xdg.configFile."TabNine".source = rice + "/.config/TabNine";

  # # services
  # services.gpg-agent.enable = false;
  # services.dunst.enable = false;

  # xdg.configFile."kitty".recursive = true;
  # xdg.configFile."kitty".source = rice + "/.config/kitty/";
  # xdg.configFile."hub".source = rice + "/secrets/hub";
  # xdg.configFile."pet/snippet.toml".source = rice + "/.config/pet/snippet.toml";
  # xdg.configFile."pet/config.toml".source = rice + "/secrets/pet/config.toml";

  # # ubuntu
  # xdg.configFile."apt/".source = rice + "/secrets/apt/";

  # home.packages = [
  #   pkgs.browsh
  #   pkgs.kitty
  #   pkgs.git-hub
  #   pkgs.pet
  #   pkgs.tmux
  #   pkgs.fzf
  #   pkgs.fd
  #   pkgs.bat
  #   pkgs.ripgrep
  #   pkgs.zoxide
  #   pkgs.exa
  #   pkgs.bandwhich
  #   pkgs.wego
  #   pkgs.duplicacy
  #   pkgs.translate-shell
  #   pkgs.rclone
  #   # theme
  #   # pkgs.input-fonts

  #   # langs
  #   pkgs.go-pup
  #   pkgs.nixfmt
  #   pkgs.golangci-lint
  #   pkgs.shfmt

  #   # python
  #   pkgs.python38Packages.howdoi
  # ];
  # programs.emacs = {
  #   enable = true;
  #   package = pkgs.emacsGcc;
  #   extraPackages = (epkgs: [ pkgs.mu pkgs.emacs-all-the-icons-fonts ]);
  # };
  # nixpkgs.overlays = [
  #   (import (builtins.fetchTarball {
  #     url =
  #       "https://github.com/nix-community/emacs-overlay/archive/80a785f1223b7c526ca2c4a81939fa1d5756b2da.tar.gz";
  #   }))
  # ];
  # programs.mu.enable = true;
}
