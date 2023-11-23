{
  console.useXkbConfig = true;

  boot = {
    zfs.forceImportRoot = false;
    initrd = {
      systemd.enable = mkDefault true;
      systemd.emergencyAccess =
        "$6$UxT9KYGGV6ik$BhH3Q.2F8x1llZQLUS1Gm4AxU7bmgZUP7pNX6Qt3qrdXUy7ZYByl5RVyKKMp/DuHZgk.RiiEXK8YVH.b2nuOO/";
    };
    loader.grub.memtest86.enable = true;
  };

  zfs-root.boot = {
    devNodes = "/dev/disk/by-id/";
    immutable.enable = mkDefault true;
  };

  security = {
    doas.enable = true;
    sudo.enable = false;
    lockKernelModules = false;
  };

}
