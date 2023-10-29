{ config, lib, pkgs, ... }:
let
  inherit (lib) mkMerge;
in {
  services.xserver = {
    layout = "ergo-keymap-qwerty";
    extraLayouts."ergo-keymap-qwerty" = {
      description = "zfs-root layout";
      languages = [ "eng" ];
      symbolsFile = ./ergo-keymap-qwerty.txt;
    };
  };
  environment.variables = { XKB_DEFAULT_LAYOUT = "ergo-keymap-qwerty"; };
  users.users = {
    mz = {
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
      programs.bash = {
        enable = true;
        initExtra =
          "if [ -f ~/.config/yc.sh ]; then source ~/.config/yc.sh; fi";
      };
      xdg = {
        configFile = {
          "sway/yc-sticky-keymap" = {
            source = ./ergo-keymap-qwerty.txt;
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
