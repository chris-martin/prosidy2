#
# Repo:    https://github.com/NixOS/nixpkgs-channels
# Branch:  nixos-unstable
# Updated: 2019-01-25
#

args:

let
  static     = builtins.getEnv "NIX_STATIC" != "";
  url        = "https://github.com/nh2/nixpkgs/archive/${commit}.tar.gz";
  commit     = "11aa987ea5b5a593c9ca7a38b391804959f905e5";
  sha256     = "1wns6051fxpgmszyxjvk7020zzhljjxqxkrnqmyd3b6fah7q4vny";
  nixpkgs    = import tarball args;
  tarball    = builtins.fetchTarball { inherit url sha256; };
in
  if static then nixpkgs.pkgsMusl else nixpkgs
