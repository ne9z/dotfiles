{ ... }:
let user-name = "yc";
in {
  imports =
    [ ../../imports/common.nix ../../imports/desktop.nix ../../imports/zh.nix ];
  networking = {
    hostName = "beijing";
    hostId = "abcd1234";
  };
  time.timeZone = "Europe/Berlin";

}
