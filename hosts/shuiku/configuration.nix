{ ... }: {
  imports = [ ../../imports/common.nix ../../imports/desktop.nix ];
  networking = {
    hostName = "shuiku";
    hostId = "38b0962b";
  };
  time.timeZone = "Europe/Berlin";
  boot.initrd.availableKernelModules = [ "i915" ];
}
