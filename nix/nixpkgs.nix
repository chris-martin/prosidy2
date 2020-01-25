#
# Repo:    https://github.com/NixOS/nixpkgs-channels
# Branch:  nixos-unstable
# Updated: 2019-01-25
#

args:

let
  static     = builtins.getEnv "NIX_STATIC" != "";
  url        = "https://github.com/NixOS/nixpkgs-channels/archive/${commit}.tar.gz";
  commit     = "05626cc86b8a8bbadae7753d2e33661400ff67de";
  sha256     = "1lx5hs2yzg21pc8fv982kdqhc5j5kxi08h8wn3plx848bcnd8jj6";
  nixpkgs    = import tarball args;
  tarball    = builtins.fetchTarball { inherit url sha256; };
in
  if static then nixpkgs.pkgsMusl else nixpkgs
