self: super:

let
in {
  userPackages = super.userPackages or { } // import ./packages.nix self super
    // super.lib.optionalAttrs (builtins.pathExists ./local.nix)
    (import ./local.nix self super) // {
      # Default packages for single-user; don't include this for multi-user
      inherit (self) cacert nix;
    } // {
      # Utilities
      nix-rebuild = super.writeShellScriptBin "nix-rebuild" ''
        set -e
        if ! command -v nix-env &>/dev/null; then
          echo "warning: nix-env was not found in PATH, add nix to userPackages" >&2
          PATH=${self.nix}/bin:$PATH
        fi
        IFS=- read -r _ oldGen _ <<<"$(readlink "$(readlink ~/.nix-profile)")"
        oldVersions=$(readlink ~/.nix-profile/package_versions || echo "/dev/null")
        nix-env -f '<nixpkgs>' -r -iA userPackages "$@"
        IFS=- read -r _ newGen _ <<<"$(readlink "$(readlink ~/.nix-profile)")"
        ${self.diffutils}/bin/diff --color -u --label "generation $oldGen" $oldVersions \
          --label "generation $newGen" ~/.nix-profile/package_versions \
          || true
      '';
      nix-what-rebuild = super.writeShellScriptBin "nix-what-rebuild" ''
        set -e
        if ! command -v nix-env &>/dev/null; then
          echo "warning: nix-env was not found in PATH, add nix to userPackages" >&2
          PATH=${self.nix}/bin:$PATH
        fi
        IFS=- read -r _ oldGen _ <<<"$(readlink "$(readlink ~/.nix-profile)")"
        oldVersions=$(readlink ~/.nix-profile/package_versions || echo "/dev/null")
        newVersions=$(nix-build --no-out-link -A userPackages.packageVersions '<nixpkgs>')
        ${self.diffutils}/bin/diff --color -u --label "generation $oldGen" "$oldVersions" \
          --label "after rebuild" "$newVersions" \
          && echo "no changes" \
          || true
      '';

      packageVersions = let
        collect = attrs:
          let
            recurse = x:
              if super.lib.isDerivation x then
                [ x ]
              else if x.recurseForDerivations or false then
                collect x
              else
                [ ];
          in super.lib.concatMap recurse (super.lib.attrValues attrs);
        versions = map (pkg: pkg.name) (collect self.userPackages);
        versionText = super.lib.strings.concatMapStrings (s: s + "\n") versions;
      in super.writeTextDir "package_versions" versionText;
    };
}
