{
  imports = [ ./configuration.nix ];
  zfs-root = { boot = { enable = false; }; };
  home-manager.users.yc = {
    wayland.windowManager.sway.config = { output."*".scale = "3"; };
  };
}
