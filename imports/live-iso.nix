{ modulesPath, config, lib, pkgs, ... }:
let
  inherit (lib) mkMerge;
  firefoxPkg = import ./firefox.nix { inherit pkgs; };
in {
  boot.initrd.availableKernelModules = [ "i915" ];
  boot.initrd.systemd.enable = false;
  imports = [ (modulesPath + "/installer/cd-dvd/installation-cd-base.nix") ];
  isoImage.squashfsCompression = "lz4";
  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
  networking = {
    wireless = {
      networks = {
        "TP-Link_48C2".psk = "77017543";
        "1203-5G".psk = "woxiangshuoqinaidemaizi";
        # public network
        # "_Free_Wifi_Berlin" = {};
      };
    };
  };
  nix.settings.substituters = lib.mkBefore [
    "https://mirror.sjtu.edu.cn/nix-channels/store"
  ];
  users.users = {
    yc = {
      # "!" means login disabled
      initialPassword = "test";
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
      programs.bash = {
        enable = true;
        initExtra =
          "if [ -f ~/.config/yc.sh ]; then source ~/.config/yc.sh; fi";
      };
      programs.git = {
        enable = true;
        userEmail = "yguo@posteo.net";
        userName = "Yǔchēn Guō 郭宇琛";
      };
      programs.ssh = {
        matchBlocks = {
          "unixpool.math.tu-berlin.de".user = "cma-1e1";
          "codeberg.org".user = "git";
          "github.com".user = "git";
          "tl.yc" = {
            user = "yc";
            port = 65222;
          };
          "3ldetowqifu5ox23snmoblv7xapkd26qyvex6fwrg6zpdwklcatq.b32.i2p" = {
            user = "yc";
            port = 65222;
            proxyCommand = "${pkgs.libressl.nc}/bin/nc -x localhost:4447 %h %p";
          };
          "ditgedyyvwsxspdmgpnzuzhj7p63snkiok54cphmvwcgnrjgw2lqgcad.onion" = {
            user = "yc";
            port = 22;
            proxyCommand = "${pkgs.libressl.nc}/bin/nc -x localhost:9050 %h %p";
          };
        };
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
    }
  ];
}
