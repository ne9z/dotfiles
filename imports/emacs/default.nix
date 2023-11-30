{ pkgs, ... }:
let
  buildEmacs = (pkgs.emacsPackagesFor pkgs.emacs29-pgtk).emacsWithPackages;
  emacsPkg = buildEmacs (epkgs:
    builtins.attrValues {
      inherit (epkgs.melpaPackages) nix-mode magit pdf-tools haskell-mode;
      inherit (epkgs.elpaPackages) auctex pyim pyim-basedict;
      inherit (epkgs.treesit-grammars) with-all-grammars;
    });
in {
  home-manager.users.yc.services = {
    emacs = {
      enable = true;
      package = emacsPkg;
      client.enable = true;
      client.arguments = [ "--create-frame" ];
      defaultEditor = true;
      startWithUserSession = "graphical";
    };
  };
  environment.systemPackages = [ emacsPkg ];
  home-manager.users.yc.xdg.configFile."emacs/init.el".source = ./init.el;
}
