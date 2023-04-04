{ config, lib, pkgs, ... }:
let
  cfg = config.zfs-root.per-user.yc.modules.virt;
  inherit (lib) mkDefault mkOption types mkIf;
in {
  options.zfs-root.per-user.yc.modules.virt.enable = mkOption {
    type = types.bool;
    default = config.zfs-root.per-user.yc.enable;
  };
  config = mkIf cfg.enable {
    virtualisation.libvirtd = {
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        runAsRoot = false;
      };
    };
    environment.systemPackages =
      builtins.attrValues { inherit (pkgs) virt-manager; };
  };
}
