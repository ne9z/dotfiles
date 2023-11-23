let
  homeBindOpts =
    [ "bind" "X-mount.mkdir" "noatime" "uid=yc" "gid=users" "mode=1700" ];
in {
  zfs-root.fileSystems.datasets."rpool/nixos/home" = "/oldroot/home";
  fileSystems = {
    "/home/yc" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "rw" "size=1G" "uid=yc" "gid=users" "mode=1700" ];
    };
    "/home/yc/Downloads" = {
      device = "/oldroot/home/yc/Downloads";
      fsType = "none";
      options = homeBindOpts;
    };
    "/home/yc/Documents" = {
      device = "/oldroot/home/yc/Documents";
      fsType = "none";
      options = homeBindOpts;
    };
    "/home/yc/nixos-config" = {
      device = "/oldroot/home/yc/nixos-config";
      fsType = "none";
      options = homeBindOpts;
    };
    "/home/yc/.gnupg" = {
      device = "/oldroot/home/yc/.gnupg";
      fsType = "none";
      options = homeBindOpts;
    };
    "/home/yc/.password-store" = {
      device = "/oldroot/home/yc/.password-store";
      fsType = "none";
      options = homeBindOpts;
    };
  };
}
