{ config, lib, pkgs, ... }:
let
  cfg = config.zfs-root.per-user.yc.modules.tex;
  inherit (lib) mkDefault mkOption types mkIf;
in {
  options.zfs-root.per-user.yc.modules.tex.enable = mkOption {
    default = config.zfs-root.per-user.yc.enable;
    type = types.bool;
  };
  config = mkIf cfg.enable {
    environment.systemPackages = builtins.attrValues {
      mytex = (pkgs.texlive.combine {
        inherit (pkgs.texlive)
          scheme-basic latexmk amsmath hyperref listings
          # maths
          collection-mathscience
          # languages
          collection-langgerman
          # pdf manipulation tool
          pdfjam pdfpages eso-pic atbegshi pdflscape
          # unicode-math
          fontspec realscripts unicode-math lualatex-math stix2-otf
          libertinus-otf
          # header and footer
          fancyhdr
          # pictures and tikz
          collection-pictures;
      });
    };
  };
}
