{ config, lib, pkgs, ... }:

let inherit (lib) types mkOption mkIf;
in {
  imports = [ ./modules ./templates ];
  options.zfs-root.per-user.yc.enable = mkOption {
    description = "enable yc options with desktop";
    type = types.bool;
    default = false;
  };
  config = {};
}
