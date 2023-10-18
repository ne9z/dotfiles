{ config, lib, pkgs, pmacs, ... }:
let
  inherit (lib) mkDefault mkOption types mkIf;
  # buildEmacs is a function that takes a set of emacs packages as
  # input
  emacsPkg = import ./emacs.nix { inherit pkgs; };
  firefoxPkg = import ./firefox.nix { inherit pkgs; };
  mytex = import ./tex.nix { inherit pkgs; };
in {
  programs.chromium = {
    enable = true;
    defaultSearchProviderEnabled = false;
    extraOpts = {
      "DefaultJavaScriptJitSetting" = 2;
      "DefaultFileSystemWriteGuardSetting" = 2;
      "DefaultLocalFontsSetting" = 2;
      "AlwaysOpenPdfExternally" = true;
      "BrowserSignin" = 0;
      "SyncDisabled" = true;
      "PasswordManagerEnabled" = false;
      "SpellcheckEnabled" = true;
      "SpellcheckLanguage" = [ "de" "en-US" ];
    };
    homepageLocation = "https://gnu.org";
  };
  security.chromiumSuidSandbox.enable = true;
  services.xserver.enable = true; # for sddm
  services.xserver.displayManager.sddm = {
    enable = true;
    wayland.enable = true;
  };
  users.mutableUsers = false;
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
      export XCURSOR_THEME=Adwaita
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
      monospace = [ "NewComputerModernMono10" "Noto Sans Mono CJK SC" ];
      sansSerif =
        [ "TeX Gyre Schola" "NewComputerModern08" "Noto Serif CJK SC" ];
      serif = [ "TeX Gyre Schola" "NewComputerModern08" "Noto Serif CJK SC" ];
    };
  };
  fonts.packages = builtins.attrValues {
    inherit (pkgs)
      noto-fonts dejavu_fonts
      # noto cjk
      noto-fonts-cjk-serif noto-fonts-cjk-sans;
  } ++ [ mytex.fonts ];
  environment.sessionVariables = {
    QT_AUTO_SCREEN_SCALE_FACTOR = "0";
    QT_ENABLE_HIGHDPI_SCALING = "0";
    QT_SCALE_FACTOR = mkDefault "2";
    GDK_DPI_SCALE = mkDefault "2";
    VAAPI_DISABLE_INTERLACE = "1";
    W3M_DIR = "$HOME/.config/w3m";
  };
  services.dictd = {
    enable = true;
    DBs = builtins.attrValues { inherit (pkgs.dictdDBs) wordnet; };
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
        # debug
        gdb
        # used with dired mode to open files
        xdg-utils;
      inherit (pkgs.hunspellDicts) en_US de_DE;
      inherit emacsPkg mytex firefoxPkg;
    };
    interactiveShellInit = ''
      e () { $EDITOR --create-frame "$@"; }
    '';
  };
  console.useXkbConfig = true;
}
