#
# Repo:    https://github.com/NixOS/nixpkgs-channels
# Branch:  nixos-unstable
# Updated: 2019-01-25
#

args:

let
  static     = builtins.getEnv "NIX_STATIC" != "";
  url        = "https://github.com/NixOS/nixpkgs-channels/archive/${commit}.tar.gz";
  commit     = "83a5ad13b743650e9fc5e79409b9e9470602fcf9";
  sha256     = "0hv6bl0r3gvz7b1mm0hm1bshxcwh7xpvj3x8aks7hfw3axvshh2r";
  nixpkgs    = import tarball args;
  tarball    = builtins.fetchTarball { inherit url sha256; };
in
  if static then nixpkgs.pkgsMusl else nixpkgs
