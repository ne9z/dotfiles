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
          scheme-basic latexmk amsmath hyperref
          # maths
          collection-mathscience
          # languages
          collection-langgerman
          # pdf manipulation tool
          pdfjam pdfpages
          # stix font
          stix2-type1
          # fontspec for lualatex and others
          fontspec lualatex-math mathtools
          # pictures and tikz
          collection-pictures;
      });
      # used for code listings with minted package
      inherit (pkgs) python3;
      inherit (pkgs.python3Packages) pygments;
    };
  };
}
