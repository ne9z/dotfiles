{ config, lib, pkgs, ... }:
let
  cfg = config.yc.modules.tex;
  inherit (lib) mkDefault mkOption types mkIf;
in {
  options.yc.modules.tex.enable = mkOption {
    default = config.yc.enable;
    type = types.bool;
  };
  config = mkIf cfg.enable {
    environment.systemPackages = builtins.attrValues {
      inherit (pkgs) poppler perl;
      mytex = (pkgs.texlive.combine {
        inherit (pkgs.texlive)
          collection-luatex
          # maths
          collection-mathscience
          # languages
          collection-langenglish collection-langgerman
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
          # convert to html
          lwarp xindy latexmk pdfcrop
          # pictures and tikz
          collection-pictures;
      });
    };
  };
}
