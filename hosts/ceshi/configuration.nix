{ pkgs, ... }: {
  imports = [
    ../../imports/common.nix
    ../../imports/desktop.nix
    ../../imports/live-iso.nix
  ];
  time.timeZone = "Asia/Shanghai";
  networking = { hostName = "ceshi"; };
}
