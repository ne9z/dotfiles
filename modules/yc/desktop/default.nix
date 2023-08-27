{ config, lib, ... }:
let
  cfg = config.yc.modules.keyboard;
  inherit (lib) mkDefault mkOption types mkIf;
in {
  options.yc.modules.keyboard.enable = mkOption {
    default = false;
    type = types.bool;
  };
  config = mkIf cfg.enable {
    imports = [ ];
    console.useXkbConfig = true;
    services.xserver = {
      layout = "yc";
      extraLayouts."yc" = {
        description = "zfs-root layout";
        languages = [ "eng" ];
        symbolsFile = ./symbols.txt;
      };
    };
    environment.variables = { XKB_DEFAULT_LAYOUT = "yc"; };
  };
}
