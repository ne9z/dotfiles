{ config, lib, pkgs, ... }:
let
  cfg = config.zfs-root.programs.sway;
  inherit (lib) mkDefault mkOption types mkIf;
in {
  options.zfs-root.programs.sway.enable = mkOption {
    description = "enable sway window manager and zfs-root favorite programs";
    type = types.bool;
    default = false;
  };
  config = mkIf (cfg.enable) {
    hardware = {
      opengl = {
        extraPackages =
          builtins.attrValues { inherit (pkgs) vaapiIntel intel-media-driver; };
        enable = true;
      };
      bluetooth = {
        enable = true;
        powerOnBoot = true;
      };
      pulseaudio.enable = false;
    };
    services = {
      blueman.enable = true;
      logind = {
        extraConfig = ''
          HandlePowerKey=suspend
        '';
        lidSwitch = "suspend";
        lidSwitchDocked = "ignore";
        lidSwitchExternalPower = "suspend";
      };
      pipewire = {
        enable = true;
        alsa.enable = true;
        pulse.enable = true;
      };
    };
    sound.enable = true;
    programs.sway = {
      extraSessionCommands = ''
        export MOZ_ENABLE_WAYLAND=1
        export QT_QPA_PLATFORM=wayland-egl
        export XCURSOR_THEME=Adwaita
        export _JAVA_AWT_WM_NONREPARENTING=1
      '';
      enable = true;
      extraPackages = builtins.attrValues {
        inherit (pkgs)
          swaylock swayidle foot gammastep brightnessctl fuzzel grim w3m
          gsettings-desktop-schemas pavucontrol waybar wl-clipboard;
      };
    };
    xdg.portal = {
      enable = true;
      wlr.enable = true;
    };
    fonts.fontconfig.defaultFonts = {
      monospace = [ "Source Code Pro" ];
      sansSerif = [ "Noto Sans Display" ];
      serif = [ "Noto Sans Display" ];
    };
    fonts.fonts = builtins.attrValues {
      inherit (pkgs) noto-fonts noto-fonts-cjk-sans source-code-pro;
    };
    environment.sessionVariables = {
      QT_AUTO_SCREEN_SCALE_FACTOR = "1";
      QT_WAYLAND_FORCE_DPI = "physical";
    };
  };
}
