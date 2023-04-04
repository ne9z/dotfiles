{ system, pkgs, ... }: {
  inherit pkgs system;
  zfs-root = {
    boot = {
      bootDevices = [ "ata-TOSHIBA_Q300._46DB5111K1MU" ];
      availableKernelModules = [ "i915" ];
    };
    networking = {
      hostName = "qinghe";
      hostId = "abcd1234";
    };
    per-user.yc = {
      templates.desktop.enable = true;
      # custom keyboard layout
      modules.keyboard.enable = true;
    };
  };
}
