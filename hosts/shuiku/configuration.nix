{ ... }: {
  networking = {
    hostName = "shuiku";
    hostId = "38b0962b";
  };
  boot.initrd.availableKernelModules = [ "i915" ];
}
