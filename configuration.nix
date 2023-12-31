# configuration in this file is shared by all hosts

{ config, pkgs, modulesPath, inputs, lib, ... }:
let
  inherit (inputs) self;
  inherit (lib) mkDefault;
in {
  # Safety mechanism: refuse to build unless everything is
  # tracked by git
  system.configurationRevision = if (self ? rev) then
    self.rev
  else
    throw "refuse to build: git tree is dirty";

  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/hardened.nix")
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

  system.stateVersion = "23.11";

  nix.registry.nixpkgs.flake = inputs.nixpkgs;

  zramSwap.enable = true;

  console.useXkbConfig = true;

  security = {
    doas.enable = true;
    sudo.enable = false;
    lockKernelModules = false;
  };
}
