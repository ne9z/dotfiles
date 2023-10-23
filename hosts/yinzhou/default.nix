{
  imports = [ ./configuration.nix ];
  zfs-root = {
    boot = {
      bootDevices = [ "nvme-SAMSUNG_MZVLV256HCHP-000H1_S2CSNA0J547878" ];
      luks.enable = false;
    };
  };
}
