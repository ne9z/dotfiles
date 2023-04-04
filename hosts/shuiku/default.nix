{ system, pkgs, ... }: {
  inherit pkgs system;
  zfs-root = {
    boot = {
      bootDevices = [ "ata-SDEZS25-480G-Z01_183517455908" ];
      availableKernelModules = [ "i915" ];
    };
    networking = {
      hostName = "shuiku";
      hostId = "38b0962b";
    };
    per-user.yc = {
      templates.desktop.enable = true;
      # custom keyboard layout
      modules.keyboard.enable = true;
    };
  };
}
