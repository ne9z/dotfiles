{ pkgs, ... }: {
  imports = [ ../../imports/profiles/live.nix ];
  time.timeZone = "Asia/Shanghai";
  networking = { hostName = "ceshi"; };
}
