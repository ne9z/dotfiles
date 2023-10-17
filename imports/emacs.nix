{ pkgs }:
let
  buildEmacs = (pkgs.emacsPackagesFor
    (pkgs.enableDebugging pkgs.emacs29-pgtk)).emacsWithPackages;
in buildEmacs (epkgs:
  builtins.attrValues {
    inherit (epkgs.melpaPackages) nix-mode magit pdf-tools;
    inherit (epkgs.elpaPackages) auctex pyim pyim-basedict;
    inherit (epkgs.treesit-grammars) with-all-grammars;
  })
