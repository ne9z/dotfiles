{
  imports = [ ./configuration.nix ];
  zfs-root = { boot = { enable = false; }; };
  environment.sessionVariables = {
    QT_WAYLAND_FORCE_DPI = "288";
    GDK_DPI_SCALE = "3";
  };
}
