{ pkgs, ... }:
let user-name = "yc";
in {
  imports = [
    ../../imports/common.nix
    ../../imports/desktop.nix
    ../../imports/yc.nix
  ];
  time.timeZone = "Europe/Berlin";
  boot.initrd.availableKernelModules = [ "nvme" ];
  networking = {
    hostName = "yinzhou";
    hostId = "e74b069d";
  };
}
