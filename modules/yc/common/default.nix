{ config, lib, pkgs, ... }:
let
  cfg = config.yc.templates.common;
  inherit (lib) mkDefault mkOption types mkIf recursiveUpdate;
in {
  options.yc.templates.common = {
    enable = mkOption {
      description = "Enable system config template by yc";
      type = types.bool;
      default = true;
    };
  };
  config = mkIf cfg.enable {
    imports = [ ./hidden.nix ./tmux.nix ];
    programs.gnupg.agent = {
      enable = true;
      pinentryFlavor = (if config.programs.sway.enable then "qt" else "tty");
      enableSSHSupport = true;
    };

    # disable the deprecated radeon driver and force enable newer amdgpu driver
    boot.kernelParams = [
      "radeon.cik_support=0"
      "radeon.si_support=0"
      "amdgpu.cik_support=1"
      "amdgpu.si_support=1"
      "amdgpu.dc=1"
    ];
    boot.blacklistedKernelModules = [ "radeon" ];
    services.tlp = {
      enable = true;
      settings = {
        BAY_POWEROFF_ON_BAT = "1";
        STOP_CHARGE_THRESH_BAT0 = "85";
        CPU_SCALING_GOVERNOR_ON_AC = "schedutil";
        CPU_SCALING_GOVERNOR_ON_BAT = "schedutil";

      };
    };

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

    nix.settings.experimental-features =
      lib.mkDefault [ "nix-command" "flakes" ];

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
        amdgpu_top intel-gpu-tools s-tui;
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
  };
}
