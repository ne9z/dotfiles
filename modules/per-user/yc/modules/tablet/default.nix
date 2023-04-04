{ config, lib, pkgs, ... }:
let
  cfg = config.zfs-root.per-user.yc.modules.tablet;
  inherit (lib) mkDefault mkOption types mkIf;
in {
  options.zfs-root.per-user.yc.modules.tablet.enable = mkOption {
    type = types.bool;
    default = config.zfs-root.per-user.yc.enable;
  };
  config = mkIf cfg.enable {
    environment.systemPackages =
      builtins.attrValues { inherit (pkgs) xournalpp; };
  };
}
