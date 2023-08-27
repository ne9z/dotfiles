{ system, pkgs, ... }: {
  inherit pkgs system;
  zfs-root = {
    boot = {
      bootDevices = [ "nvme-SAMSUNG_MZVLV256HCHP-000H1_S2CSNA0J547878" ];
    };
  };
  yc = { templates.desktop.enable = true; };
}
