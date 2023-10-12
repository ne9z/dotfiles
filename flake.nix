{
  inputs = {
    # https://channels.nixos.org/nixpkgs-unstable/git-revision
    # https://channels.nixos.org/nixos-unstable/git-revision
    # using this one
    # https://channels.nixos.org/nixos-unstable-small/git-revision
    nixpkgs.url =
      "github:nixos/nixpkgs/349bdd9653c42f1793d338b43aefe08883c5ebee";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pmacs.url = "github:nixos/nixpkgs/349bdd9653c42f1793d338b43aefe08883c5ebee";
  };

  outputs = { self, nixpkgs, home-manager, pmacs }@inputs:
    let
      mkHost = hostName: system:
        nixpkgs.lib.nixosSystem {
          system = system;
          pkgs = nixpkgs.legacyPackages.${system};

          specialArgs = {
            pmacs = pmacs.legacyPackages.${system};
            inherit inputs;
          };

          modules = [
            ./modules

            (import ./hosts/${hostName})

            # Module 3: home-manager
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
            }
          ];
        };
    in {
      nixosConfigurations = {
        exampleHost = mkHost "exampleHost" "x86_64-linux";
        qinghe = mkHost "qinghe" "x86_64-linux";
        tieling = mkHost "tieling" "x86_64-linux";
        yinzhou = mkHost "yinzhou" "x86_64-linux";
        shuiku = mkHost "shuiku" "x86_64-linux";
      };
    };
}
