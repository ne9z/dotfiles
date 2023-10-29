{
  imports = [ ./configuration.nix ];
  zfs-root = {
    boot = {
      bootDevices = [ "ata-Micron_5100_MTFDDAK480TBY_18271D3D0DD1" ];
      luks.enable = true;
    };
  };
}
