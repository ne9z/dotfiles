{ system, pkgs, ... }: {
  inherit pkgs system;
  zfs-root = {
    boot = { bootDevices = [ "ata-SDEZS25-480G-Z01_183517455908" ]; };
  };
  yc = {
    templates.desktop.enable = true;
    # custom keyboard layout
    modules.keyboard.enable = true;
  };
}
