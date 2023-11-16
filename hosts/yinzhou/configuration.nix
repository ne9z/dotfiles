{ pkgs, ... }:
let user-name = "yc";
in {
  imports =
    [ ../../imports/common.nix ../../imports/desktop.nix ../../imports/yc.nix ];
  time.timeZone = "Europe/Berlin";
  boot.initrd.availableKernelModules = [ "i915" ];
  environment.sessionVariables = {
    QT_WAYLAND_FORCE_DPI = "96";
    GDK_DPI_SCALE = "1";
  };
  networking = {
    hostName = "yinzhou";
    hostId = "e74b069d";
  };
}
