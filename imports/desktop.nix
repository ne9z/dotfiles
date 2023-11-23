{ config, lib, pkgs, ... }:
let inherit (lib) mkDefault mkOption types mkIf;
in {
  hardware = {
    opengl = {
      extraPackages =
        builtins.attrValues { inherit (pkgs) vaapiIntel intel-media-driver; };
      enable = true;
    };
    pulseaudio.enable = mkDefault false;
  };
  sound.enable = true;
  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };
  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      package = pkgs.qemu_kvm;
      runAsRoot = false;
    };
  };
}
