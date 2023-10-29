{ config, lib, pkgs, ... }:
let inherit (lib) mkMerge;
in {
  services.xserver = {
    layout = "ergo-keymap-qwerty";
    extraLayouts."ergo-keymap-qwerty" = {
      description = "zfs-root layout";
      languages = [ "eng" ];
      symbolsFile = ./ergo-keymap-qwerty.txt;
    };
  };
  zfs-root.boot.immutable = lib.mkForce false;
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
    }
  ];
}
