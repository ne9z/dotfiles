{ pkgs, ... }: {
  imports = [ ../../imports/common.nix ../../imports/desktop.nix ];
  networking = {
    hostName = "yinzhou";
    hostId = "e74b069d";
  };
}
