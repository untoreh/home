import (builtins.fetchTarball {
  url = "https://github.com/nix-community/emacs-overlay/archive/${
      builtins.getEnv "EMACS_COMMIT"
    }.tar.gz";
})
