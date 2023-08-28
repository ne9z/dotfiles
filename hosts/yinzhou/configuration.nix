{ pkgs, ... }: {
  imports = [ ../../imports/common.nix ../../imports/zh.nix ];
  time.timeZone = "Europe/Berlin";
  boot.initrd.availableKernelModules = [ "nvme" ];
  networking = {
    hostName = "yinzhou";
    hostId = "e74b069d";
  };
}
