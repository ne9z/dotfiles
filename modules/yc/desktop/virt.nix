{ config, lib, pkgs, ... }:
let
  cfg = config.yc.modules.virt;
  inherit (lib) mkDefault mkOption types mkIf;
in {
  options.yc.modules.virt.enable = mkOption {
    type = types.bool;
    default = config.yc.enable;
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
