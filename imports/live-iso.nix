{ modulesPath, config, lib, pkgs, ... }: {
  imports = [ (modulesPath + "/installer/cd-dvd/installation-cd-base.nix") ];

  boot.initrd.availableKernelModules = [ "i915" ];
  boot.initrd.systemd.enable = false;
  isoImage.squashfsCompression = "lz4";
  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
}
