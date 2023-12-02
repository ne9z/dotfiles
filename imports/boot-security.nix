{
  boot = {
    zfs.forceImportRoot = false;
    initrd = {
      systemd.enable = true;
      systemd.emergencyAccess =
        "$6$UxT9KYGGV6ik$BhH3Q.2F8x1llZQLUS1Gm4AxU7bmgZUP7pNX6Qt3qrdXUy7ZYByl5RVyKKMp/DuHZgk.RiiEXK8YVH.b2nuOO/";
    };
    loader.grub.memtest86.enable = true;
    # disable the deprecated radeon driver and force enable newer amdgpu driver
    kernelParams = [
      "radeon.cik_support=0"
      "radeon.si_support=0"
      "amdgpu.cik_support=1"
      "amdgpu.si_support=1"
      "amdgpu.dc=1"
    ];
    blacklistedKernelModules = [ "radeon" ];
  };

  zfs-root.boot = {
    devNodes = "/dev/disk/by-id/";
    immutable.enable = true;
  };
}
