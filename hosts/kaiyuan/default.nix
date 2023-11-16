{
  imports = [ ./configuration.nix ];
  zfs-root = {
    boot = {
      bootDevices = [ "ata-SDEZS25-480G-Z01_183517455908" ];
      luks.enable = true;
    };
  };
}
