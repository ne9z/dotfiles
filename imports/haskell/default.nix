{ pkgs, config, ... }:
let
  myHaskellPkg = (hp: builtins.attrValues { inherit (hp) cabal-install; });
  myHaskellEnv = pkgs.haskellPackages.ghcWithHoogle myHaskellPkg;
in {
  services = {
    hoogle = {
      enable = true;
      packages = myHaskellPkg;
      haskellPackages = pkgs.haskellPackages;
    };
  };
  environment.systemPackages = builtins.attrValues {
    # https://haskell4nix.readthedocs.io/nixpkgs-users-guide.html
    inherit myHaskellEnv;
    inherit (pkgs) haskell-language-server;
  };
}
