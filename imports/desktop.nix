{ config, lib, pkgs, ... }:
let
  inherit (lib) mkDefault mkOption types mkIf;
  # buildEmacs is a function that takes a set of emacs packages as
  # input
  emacsPkg = import ./emacs.nix { inherit pkgs; };
  mytex = import ./tex.nix { inherit pkgs; };
in {
  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = (if config.programs.sway.enable then "qt" else "tty");
    enableSSHSupport = true;
  };
  networking = {
    wireless = {
      enable = true;
      allowAuxiliaryImperativeNetworks = true;
      networks = {
        # configured in yc.nix
      };
      userControlled = {
        enable = true;
        group = "wheel";
      };
    };
  };
  hardware = {
    opengl = {
      extraPackages =
        builtins.attrValues { inherit (pkgs) vaapiIntel intel-media-driver; };
      enable = true;
    };
    pulseaudio.enable = mkDefault false;
  };
  services.xserver.enable = true;
  services.xserver.displayManager.sddm = {
    enable = true;
    wayland.enable = true;
  };
  services = {
    logind = {
      extraConfig = ''
        HandlePowerKey=suspend
      '';
      lidSwitch = "suspend";
      lidSwitchDocked = "ignore";
      lidSwitchExternalPower = "suspend";
    };
    pipewire = {
      enable = mkDefault true;
      alsa.enable = true;
      pulse.enable = true;
    };
  };
  sound.enable = true;
  security.chromiumSuidSandbox.enable = true;
  programs.chromium = {
    enable = true;
    homepageLocation = "chrome://settings/content/javascript";
    extraOpts = {
      "BookmarkBarEnabled" = false;
      "DefaultFileSystemWriteGuardSetting" = 2;
      "DefaultFileSystemReadGuardSetting" = 2;
      "DefaultJavaScriptJitSetting" = 2;
      "AlwaysOpenPdfExternally" = true;
      "HttpsUpgradesEnabled" = true;
      "HighEfficiencyModeEnabled" = true;
      # set to int 2 to disable
      "ShowHomeButton" = true;
      "HomepageLocation" = "chrome://settings/content/javascript";
      "NewTabPageLocation" = "https://lite.duckduckgo.com/";
      "DefaultInsecureContentSetting" = 2;
      "DefaultNotificationsSetting" = 2;
      "DefaultWebUsbGuardSetting" = 2;
      "DefaultPopupsSetting" = 1;
    };
  };
  programs.sway = {
    extraSessionCommands = ''
      export MOZ_ENABLE_WAYLAND=1
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';
    enable = true;
    extraPackages = builtins.attrValues {
      inherit (pkgs)
        swaylock swayidle foot gammastep wl-gammactl brightnessctl fuzzel grim
        libva-utils w3m gsettings-desktop-schemas pavucontrol waybar
        wl-clipboard wf-recorder;
    };
    # must be enabled, or else many programs will crash
    wrapperFeatures.gtk = true;
  };
  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };
  fonts.fontconfig = {
    localConf = builtins.readFile ./fontconfig.xml;
    defaultFonts = {
      emoji = [ "Noto Color Emoji" ];
      monospace =
        [ "DejaVu Sans Mono" "Noto Sans Mono CJK SC" "Noto Sans Mono" ];
      sansSerif = [
        "TeX Gyre Schola"
        "NewComputerModern08"
        "Noto Serif CJK SC"
        "Noto Serif"
      ];
      serif = [
        "TeX Gyre Schola"
        "NewComputerModern08"
        "Noto Serif CJK SC"
        "Noto Serif"
      ];
    };
  };
  fonts.packages = builtins.attrValues {
    inherit (pkgs)
      noto-fonts dejavu_fonts
      # noto cjk
      noto-fonts-cjk-serif noto-fonts-cjk-sans;
  } ++ [ mytex.fonts ];
  environment.sessionVariables = {
    QT_WAYLAND_FORCE_DPI = mkDefault "192";
    GDK_DPI_SCALE = mkDefault "2";
    VAAPI_DISABLE_INTERLACE = "1";
    W3M_DIR = "$HOME/.config/w3m";
    XCURSOR_THEME = "Adwaita";
    XCURSOR_SIZE = "48";
  };
  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      package = pkgs.qemu_kvm;
      runAsRoot = false;
    };
  };
  environment = {
    systemPackages = builtins.attrValues {
      inherit (pkgs) virt-manager;
      inherit (pkgs) poppler perl;
      inherit (pkgs)
      # for use with emacs preview-latex
        ghostscript
        # spell checkers
        hunspell
        # used with dired mode to open files
        xdg-utils;
      inherit (pkgs.hunspellDicts) en_US de_DE;
      inherit emacsPkg mytex;
    };
    interactiveShellInit = ''
      e () { $EDITOR --create-frame "$@"; }
    '';
  };
  console.useXkbConfig = true;
}
