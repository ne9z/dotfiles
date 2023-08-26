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
          collection-luatex
          # maths
          collection-mathscience
          # languages
          collection-langenglish
          collection-langgerman
          collection-langcjk # base of cjk
          collection-langchinese
          # pdf manipulation tool
          pdfjam pdfpages eso-pic atbegshi pdflscape
          # unicode-math
          fontspec realscripts unicode-math lualatex-math stix2-otf
          libertinus-fonts libertinus libertinus-otf
          # newcomputermodern
          fontsetup newcomputermodern
          # header and footer
          fancyhdr
          # pictures and tikz
          collection-pictures;
      });
    };
  };
}
