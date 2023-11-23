{ config, lib, pkgs, ... }:
let
  inherit (lib) mkDefault mkOption types mkIf;
  # buildEmacs is a function that takes a set of emacs packages as
  # input
  emacsPkg = import ./emacs.nix { inherit pkgs; };
  mytex = import ./tex.nix { inherit pkgs; };
  ycFontsConf =
    pkgs.writeText "fc-56-yc-fonts.conf" (builtins.readFile ./fontconfig.xml);
  confPkg = pkgs.runCommand "fontconfig-conf" { preferLocalBuild = true; } ''
    dst=$out/etc/fonts/conf.d
    mkdir -p $dst

    # 56-yc-fonts.conf
    ln -s ${ycFontsConf} $dst/56-yc-fonts.conf
  '';
in {
  security.apparmor.includes."abstractions/fonts" = ''
    # 56-yc-fonts.conf
     r ${ycFontsConf},
  '';
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
  sound.enable = true;
  security.chromiumSuidSandbox.enable = true;
  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };
  fonts.fontconfig = {
    defaultFonts = {
      emoji = [ "Noto Color Emoji" ];
      monospace =
        [ "DejaVu Sans Mono" "Noto Sans Mono" "Noto Sans Mono CJK SC" ];
      sansSerif = [ "TeX Gyre Schola" "Noto Serif" "Noto Sans CJK SC" ];
      serif = [ "TeX Gyre Schola" "Noto Serif" "Noto Sans CJK SC" ];
    };
    confPackages = [ confPkg ];
  };
  fonts.packages = builtins.attrValues {
    inherit (pkgs)
      noto-fonts dejavu_fonts
      # noto cjk
      noto-fonts-cjk-serif noto-fonts-cjk-sans;
  } ++ [ mytex.fonts ];
  environment.sessionVariables = {
    VAAPI_DISABLE_INTERLACE = "1";
    W3M_DIR = "$HOME/.config/w3m";
    XCURSOR_THEME = "Adwaita";
    XCURSOR_SIZE = "16";
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
