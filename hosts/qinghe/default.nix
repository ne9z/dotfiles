{ system, pkgs, ... }: {
  inherit pkgs system;
  zfs-root = {
    boot = {
      bootDevices = [
        "ata-TOSHIBA_Q300._46DB5111K1MU"
        "ata-Micron_5100_MTFDDAK480TBY_18271D3D0DD1"
      ];
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
