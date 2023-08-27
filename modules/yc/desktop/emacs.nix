{ config, lib, pkgs, ... }:
let
  cfg = config.yc.modules.emacs;
  inherit (lib) mkDefault mkOption types mkIf;
  # buildEmacs is a function that takes a set of emacs packages as input
  buildEmacs = (pkgs.emacsPackagesFor pkgs.emacs29-nox).emacsWithPackages;
  emacsPkg = buildEmacs (epkgs:
    builtins.attrValues {
      inherit (epkgs.melpaPackages) nix-mode magit;
      inherit (epkgs.elpaPackages) auctex pyim pyim-basedict;
      inherit (epkgs.treesit-grammars) with-all-grammars;
    });
in {
  options.yc.modules.emacs = {
    enable = mkOption {
      type = types.bool;
      default = config.yc.enable;
    };
    extraPackages = mkOption {
      description = "normal software packages that emacs depends to run";
      type = types.listOf types.package;
      default = builtins.attrValues {
        inherit (pkgs)
        # spell checkers
          enchant nuspell
          # used with dired mode to open files
          xdg-utils;
        inherit (pkgs.hunspellDicts) en_US de_DE;
        inherit emacsPkg;
      };
    };
  };
  config = mkIf (cfg.enable) {
    services.dictd = {
      enable = true;
      DBs = builtins.attrValues { inherit (pkgs.dictdDBs) wordnet; };
    };
    environment = {
      systemPackages = cfg.extraPackages;
      interactiveShellInit = ''
        export EDITOR="emacsclient --alternate-editor= --create-frame -nw"
        e () { $EDITOR "$@"; }
      '';
    };
  };
}
