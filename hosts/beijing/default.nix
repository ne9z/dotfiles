{
  imports = [ ./configuration.nix ];
  zfs-root = {
    boot = {
      bootDevices = [ "ata-TOSHIBA_Q300._46DB5111K1MU" ];
      luks.enable = false;
    };
  };
}
