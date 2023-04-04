{ config, lib, pkgs, ... }:
let
  cfg = config.zfs-root.per-user.yc.templates.desktop;
  inherit (lib) mkDefault mkOption types mkIf;
in {
  options.zfs-root.per-user.yc.templates.desktop = {
    enable = mkOption {
      description = "Enable system config template by yc";
      type = types.bool;
      default = false;
    };
  };
  config = mkIf cfg.enable {
    zfs-root = {
      boot = {
        devNodes = "/dev/disk/by-id/";
        immutable = true;
      };

      users = {
        yc = {
          # "!" means login disabled
          initialHashedPassword =
            "$6$UxT9KYGGV6ik$BhH3Q.2F8x1llZQLUS1Gm4AxU7bmgZUP7pNX6Qt3qrdXUy7ZYByl5RVyKKMp/DuHZgk.RiiEXK8YVH.b2nuOO/";
          description = "Yuchen Guo";
          # a default group must be set
          extraGroups = [
            # use doas
            "wheel"
            # manage VMs
            "libvirtd"
            # manage network
            "networkmanager"
            # connect to /dev/ttyUSB0
            "dialout"
          ];
          packages = builtins.attrValues {
            inherit (pkgs)
              ffmpeg mg nixfmt qrencode minicom zathura jmtpfs
              gpxsee
              # pdf processor in Go
              pdfcpu
              # pdf manipulation suite in C++
              # https://qpdf.readthedocs.io/en/stable/
              qpdf;
          };
          isNormalUser = true;
        };
      };
      networking = {
        timeZone = "Europe/Berlin";
        networkmanager.wirelessNetworks = {
          "TP-Link_48C2" = "77017543";
          "1203-5G" = "hallo stranger";
        };
      };

      per-user.yc.enable = true;
    };
  };
}
