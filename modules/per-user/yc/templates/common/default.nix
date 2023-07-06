{ config, lib, pkgs, ... }:
let
  cfg = config.zfs-root.per-user.yc.templates.common;
  inherit (lib) mkDefault mkOption types mkIf recursiveUpdate;
in {
  options.zfs-root.per-user.yc.templates.common = {
    enable = mkOption {
      description = "Enable system config template by yc";
      type = types.bool;
      default = false;
    };
  };
  config = mkIf cfg.enable {
  nixpkgs.config.zathura.useMupdf = true;

  # Enable NetworkManager for wireless networking,
  # You can configure networking with "nmtui" command.
  networking.useDHCP = lib.mkDefault true;
  networking.networkmanager.enable = lib.mkDefault false;

  services.openssh = {
    enable = lib.mkDefault true;
    settings = { PasswordAuthentication = lib.mkDefault false; };
  };

  boot.zfs.forceImportRoot = lib.mkDefault false;

  nix.settings.experimental-features = lib.mkDefault [ "nix-command" "flakes" ];

  programs.git.enable = true;

  services.logrotate.checkConfig = false;
  security = {
    doas.enable = lib.mkDefault true;
    sudo.enable = lib.mkDefault false;
  };

  environment.systemPackages = builtins.attrValues {
    inherit (pkgs)
      mg # emacs-like editor
      parted # other programs
    ;
  };

  security = {
    allowSimultaneousMultithreading = false;
    lockKernelModules = false;
    apparmor.enable = true;
  };
  environment.memoryAllocator.provider = "libc";

  # disable gc which always deletes downloaded nixpkg cache
  nix = {
    gc = {
      automatic = false;
      options = "--delete-old";
    };
  };
}
