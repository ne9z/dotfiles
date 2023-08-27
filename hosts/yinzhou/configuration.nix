{ pkgs, ... }: {
  imports = [ ../../imports/common.nix ../../imports/desktop.nix ];
  boot.initrd.availableKernelModules = [ "nvme" ];
  networking = {
    hostName = "yinzhou";
    hostId = "e74b069d";
  };
}
