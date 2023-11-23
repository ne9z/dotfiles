{ config, pkgs, lib, inputs, modulesPath, ... }:
let
  inherit (lib) mkMerge mapAttrsToList mkDefault;
  inherit (inputs) self nixpkgs;
in {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/hardened.nix")
  ];

  boot.zfs.forceImportRoot = false;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

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

  system.stateVersion = "23.05";

  nix.registry.nixpkgs.flake = inputs.nixpkgs;

  zramSwap.enable = true;

  boot.initrd.systemd.enable = mkDefault true;
  # workaround for hardened profile

  environment.etc."wpa_supplicant.conf".text = "#";

  security.lockKernelModules = false;

  boot.initrd.systemd.emergencyAccess =
    "$6$UxT9KYGGV6ik$BhH3Q.2F8x1llZQLUS1Gm4AxU7bmgZUP7pNX6Qt3qrdXUy7ZYByl5RVyKKMp/DuHZgk.RiiEXK8YVH.b2nuOO/";

  networking = {
    useDHCP = mkDefault true;
    useNetworkd = mkDefault true;
    hosts = { "200:8bcd:55f4:becc:4d85:2fa6:2ed2:5eba" = [ "tl.yc" ]; };
    nameservers = [ "::1" ];
  };

  zfs-root.boot.devNodes = "/dev/disk/by-id/";
  zfs-root.boot.immutable.enable = mkDefault true;

  users.mutableUsers = false;

  boot.loader.grub.memtest86.enable = true;

  environment.memoryAllocator.provider = "libc";

}
