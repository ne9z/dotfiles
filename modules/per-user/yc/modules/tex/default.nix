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
    environment.systemPackages = [
      (pkgs.texlive.combine {
        inherit (pkgs.texlive)
        # necessary for org-mode
          scheme-basic dvipng latexmk wrapfig amsmath ulem hyperref capt-of
          # times like font
          newtx xkeyval xstring fontaxes
          # maths
          collection-mathscience
          # languages
          collection-langgerman
          # pdf manipulation tool
          pdfjam pdfpages
          # pictures and tikz
          collection-pictures;
      })
    ];
  };
}
