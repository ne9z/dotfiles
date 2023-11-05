# configuration in this file is shared by all hosts

{ pkgs, pkgs-unstable, inputs, lib, ... }:
let
  inherit (inputs) self;
  inherit (lib) mkDefault;
in {
  ## enable ZFS auto snapshot on datasets
  ## You need to set the auto snapshot property to "true"
  ## on datasets for this to work, such as
  # zfs set com.sun:auto-snapshot=true rpool/nixos/home
  services.zfs = {
    autoSnapshot = {
      enable = true;
      flags = "-k -p --utc";
      monthly = 48;
    };
  };

  boot.zfs.forceImportRoot = false;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  programs.git.enable = true;

  security = {
    doas.enable = true;
    sudo.enable = false;
  };

  environment.systemPackages = builtins.attrValues {
    inherit (pkgs)
      mg # emacs-like editor
      jq # other programs
    ;
    # By default, the system will only use packages from the
    # stable channel. i.e.
    # inherit (pkg) my-favorite-stable-package;
    # You can selectively install packages
    # from the unstable channel. Such as
    # inherit (pkgs-unstable) my-favorite-unstable-package;
    # You can also add more
    # channels to pin package version.
  };

  # Safety mechanism: refuse to build unless everything is
  # tracked by git
  system.configurationRevision = if (self ? rev) then
    self.rev
  else
    throw "refuse to build: git tree is dirty";

  system.stateVersion = "23.05";

  # outside template
  nix.registry.nixpkgs.flake = inputs.nixpkgs;
  zfs-root.boot.devNodes = "/dev/disk/by-id/";
  zfs-root.boot.immutable.enable = mkDefault true;
}
