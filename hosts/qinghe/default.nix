{
  imports = [ ./configuration.nix ];
  zfs-root = {
    boot = {
      bootDevices = [
        "ata-TOSHIBA_Q300._46DB5111K1MU"
        "ata-Micron_5100_MTFDDAK480TBY_18271D3D0DD1"
      ];
    };
  };
}
