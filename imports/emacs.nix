{ pkgs }:
let buildEmacs = (pkgs.emacsPackagesFor pkgs.emacs29-pgtk).emacsWithPackages;
in buildEmacs (epkgs:
  builtins.attrValues {
    inherit (epkgs.melpaPackages) nix-mode magit;
    inherit (epkgs.elpaPackages) auctex pyim pyim-basedict pdf-tools;
    inherit (epkgs.treesit-grammars) with-all-grammars;
  })
