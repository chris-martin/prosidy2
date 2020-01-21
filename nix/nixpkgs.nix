#
# Repo: https://github.com/NixOS/nixpkgs-channels
# Branch: nixos-19.09-small
#

args:

let
  static     = builtins.getEnv "NIX_STATIC" != "";
  archiveUrl = "https://github.com/NixOS/nixpkgs-channels/archive/${commit}.tar.gz";
  commit     = "d14cea0dec2dd59e19457180feef315054ba8c57";
  nixpkgs    = import tarball args;
  tarball    = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/d14cea0dec2dd59e19457180feef315054ba8c57.tar.gz";
    sha256 = "1jmfj8az48ifli39ww1yxhi489pfkcnb5zmiv6x117v37zbv3mr5";
  };
in
  if static then nixpkgs.static else nixpkgs

