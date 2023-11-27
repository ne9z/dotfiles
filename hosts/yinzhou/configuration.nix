{ ... }: {
  imports = [ ../../imports/profiles/desktop.nix ];
  networking = {
    hostName = "yinzhou";
    hostId = "e74b069d";
  };
  time.timeZone = "Europe/Berlin";
  home-manager.users.yc = {
    wayland.windowManager.sway.config = { output."*".scale = "2"; };
  };
}
