{ config, lib, pkgs, ... }:
let inherit (lib) mkMerge;
in {
  networking = {
    wireless = {
      environmentFile = "/home/mz/Documents/wifipass.txt";
      networks = {
        "TP-Link_48C2".psk = "77017543";
        "eduroam" = {
          authProtocols = [ "WPA-EAP" ];
          auth = ''
            eap=PEAP
            ca_cert="/etc/ssl/certs/ca-certificates.crt"
            phase2="auth=MSCHAPV2"
            identity="meng.zhang@tu-berlin.de"
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
  services.xserver = {
    layout = "ergo-keymap-qwerty";
    extraLayouts."ergo-keymap-qwerty" = {
      description = "zfs-root layout";
      languages = [ "eng" ];
      symbolsFile = ./ergo-keymap-qwerty.txt;
    };
  };
  services.yggdrasil.persistentKeys = true;
  zfs-root.boot.immutable = false;
  environment.variables = { XKB_DEFAULT_LAYOUT = "ergo-keymap-qwerty"; };
  users.users = {
    mz = {
      packages = [ pkgs.firefox-esr ];
      # "!" means login disabled
      initialPassword = "mz";
      description = "Meng Zhang";
      # a default group must be set
      extraGroups = [
        # use doas
        "wheel"
        # manage VMs
      ];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICkDT9xZLh+lHc6Z60oLZlLjzOcP39B3D7ptV6xSzAhu openpgp:0x464B6BB1"
      ];
      isNormalUser = true;
      uid = 1000;
    };
  };
  home-manager.users.mz = mkMerge [
    (import ./home-manager.nix { inherit config lib pkgs; })
    {
      home = {
        username = "mz";
        homeDirectory = "/home/mz";
        stateVersion = config.system.stateVersion;
      };
      programs.git = {
        enable = true;
        userEmail = "user@example.com";
        userName = "Meng Zhang";
      };
      programs.bash = { enable = true; };
      xdg = {
        configFile = {
          "sway/yc-sticky-keymap" = {
            source = ./not-nix-config-files/swaywm-emacs-ergo-keymap-qwerty.txt;
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
