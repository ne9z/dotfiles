{
  imports = [ ./configuration.nix ];
  zfs-root = { boot = { enable = false; }; };
}
