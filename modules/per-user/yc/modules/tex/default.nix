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
        # necessary for org-mode
          scheme-basic dvipng latexmk wrapfig amsmath ulem hyperref capt-of
          # times like font
          stix2-otf libertinus-fonts
          # maths
          collection-mathscience
          # languages
          collection-langgerman
          # pdf manipulation tool
          pdfjam pdfpages
          # code listings
          minted fvextra xifthen kvoptions fancyvrb upquote float ifplatform
          pdftexcmds etoolbox xcolor lineno framed catchfile shellesc
          # fontspec for lualatex and others
          fontspec realscripts unicode-math lualatex-math
          # pictures and tikz
          collection-pictures;
      });
      # used for code listings with minted package
      inherit (pkgs) python3;
      inherit (pkgs.python3Packages) pygments;
    };
  };
}
