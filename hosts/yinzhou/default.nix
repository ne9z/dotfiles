{
  imports = [ ./configuration.nix ];
  zfs-root = {
    boot = {
      bootDevices = [ "ata-MK0100GCTYU_BTTV508202HV100FGN" ];
      luks.enable = true;
    };
  };
}
