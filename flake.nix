{
  inputs = {
    # https://channels.nixos.org/nixpkgs-unstable/git-revision
    # https://channels.nixos.org/nixos-unstable/git-revision
    # using this one
    # https://channels.nixos.org/nixos-unstable-small/git-revision
    nixpkgs.url = "nixpkgs/52f7404b62181b4ef439bb644b3dfa58e9eb2ce7";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pmacs.url = "nixpkgs/52f7404b62181b4ef439bb644b3dfa58e9eb2ce7";
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

            ./hosts/${hostName}

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
