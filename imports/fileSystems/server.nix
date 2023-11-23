{
  zfs-root.fileSystems.datasets = {
    "rpool/data/file" = "/home";
    "rpool/nixos/var/lib" = "/var/lib";
  };
  fileSystems = {
    "/tmp/BitTorrent" = {
      device = "rpool/data/bt";
      fsType = "zfs";
      options = [ "noatime" "X-mount.mkdir=755" ];
    };
  };

}
