{ system, pkgs, ... }: {
  inherit pkgs system;
  zfs-root = {
    boot = {
      bootDevices = [ "nvme-SAMSUNG_MZVLV256HCHP-000H1_S2CSNA0J547878" ];
    };
    networking = {
      hostName = "yinzhou";
      hostId = "e74b069d";
    };
    per-user.yc = {
      templates.desktop.enable = true;
      # custom keyboard layout
      modules.keyboard.enable = true;
    };
  };
}
