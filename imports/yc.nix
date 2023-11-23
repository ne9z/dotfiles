{ config, lib, pkgs, ... }:
let
  inherit (lib) mkMerge;
  firefoxPkg = import ./firefox.nix { inherit pkgs; };
  homeBindOpts =
    [ "bind" "X-mount.mkdir" "noatime" "uid=yc" "gid=users" "mode=1700" ];
in {
  zfs-root.fileSystems.datasets."rpool/nixos/home" = "/oldroot/home";
  networking = {
    wireless = {
      environmentFile = "/home/yc/Documents/wifipass.txt";
      networks = {
        "TP-Link_48C2".psk = "77017543";
        "eduroam" = {
          authProtocols = [ "WPA-EAP" ];
          auth = ''
            eap=PEAP
            ca_cert="/etc/ssl/certs/ca-certificates.crt"
            phase2="auth=MSCHAPV2"
            identity="yguo@tu-berlin.de"
            domain_suffix_match="tu-berlin.de"
            anonymous_identity="wlan@tu-berlin.de"
            password="@PASS_TU_BERLIN@"
          '';
        };
        # public network
        # "_Free_Wifi_Berlin" = {};
      };
    };
  };
  fileSystems = {
    "/home/yc" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "rw" "size=1G" "uid=yc" "gid=users" "mode=1700" ];
    };
    "/home/yc/Downloads" = {
      device = "/oldroot/home/yc/Downloads";
      fsType = "none";
      options = homeBindOpts;
    };
    "/home/yc/Documents" = {
      device = "/oldroot/home/yc/Documents";
      fsType = "none";
      options = homeBindOpts;
    };
    "/home/yc/nixos-config" = {
      device = "/oldroot/home/yc/nixos-config";
      fsType = "none";
      options = homeBindOpts;
    };
    "/home/yc/.gnupg" = {
      device = "/oldroot/home/yc/.gnupg";
      fsType = "none";
      options = homeBindOpts;
    };
    "/home/yc/.password-store" = {
      device = "/oldroot/home/yc/.password-store";
      fsType = "none";
      options = homeBindOpts;
    };
  };
  environment.variables = { XKB_DEFAULT_LAYOUT = "yc"; };
  users.users = {
    yc = {
      # "!" means login disabled
      initialHashedPassword =
        "$6$UxT9KYGGV6ik$BhH3Q.2F8x1llZQLUS1Gm4AxU7bmgZUP7pNX6Qt3qrdXUy7ZYByl5RVyKKMp/DuHZgk.RiiEXK8YVH.b2nuOO/";
      description = "Yǔchēn Guō 郭宇琛";
      # a default group must be set
      extraGroups = [
        # use doas
        "wheel"
        # manage VMs
        "libvirtd"
      ];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICkDT9xZLh+lHc6Z60oLZlLjzOcP39B3D7ptV6xSzAhu openpgp:0x464B6BB1"
      ];
      isNormalUser = true;
      uid = 1000;
    };
  };
  home-manager.users.yc = mkMerge [
    (import ./home-manager.nix { inherit config lib pkgs; })
    {
      home = {
        packages = [ firefoxPkg ];
        username = "yc";
        homeDirectory = "/home/yc";
        stateVersion = config.system.stateVersion;
      };

      xdg = {
        configFile = {
          "sway/yc-sticky-keymap" = {
            source = ./not-nix-config-files/sway-yc-sticky-keymap;
          };
          "yc.sh" = { source = ./not-nix-config-files/bashrc-config.sh; };
          "w3m/bookmark.html" = {
            source = ./not-nix-config-files/w3m-bookmark.html;
          };
        };

      };
      wayland.windowManager.sway = {
        config.input = {
          "type:keyboard" = {
            xkb_file = "$HOME/.config/sway/yc-sticky-keymap";
          };
        };
      };
    }
  ];
}
