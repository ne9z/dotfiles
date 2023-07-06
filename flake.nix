{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    home-manager.url = "github:nix-community/home-manager/release-23.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager }:
    let
      lib = nixpkgs.lib;
      mkHost = { zfs-root, pkgs, system, ... }:
        lib.nixosSystem {
          inherit system;
          modules = [
            ./modules
            (({ zfs-root, pkgs, lib, ... }: {
              inherit zfs-root;
              system.configurationRevision = if (self ? rev) then
                self.rev
              else
                throw "refuse to build: git tree is dirty";
              system.stateVersion = "23.05";

              imports = [
                "${nixpkgs}/nixos/modules/installer/scan/not-detected.nix"
                "${nixpkgs}/nixos/modules/profiles/hardened.nix"
              ];

            }) { inherit zfs-root pkgs lib; })
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
            }
          ];
        };
    in {
      nixosConfigurations = {
        exampleHost = let
          system = "x86_64-linux";
          pkgs = nixpkgs.legacyPackages.${system};
        in mkHost (import ./hosts/exampleHost { inherit system pkgs; });

        qinghe = let
          system = "x86_64-linux";
          pkgs = nixpkgs.legacyPackages.${system};
        in mkHost (import ./hosts/qinghe { inherit system pkgs; });

        tieling = let
          system = "x86_64-linux";
          pkgs = nixpkgs.legacyPackages.${system};
        in mkHost (import ./hosts/tieling { inherit system pkgs; });

        yinzhou = let
          system = "x86_64-linux";
          pkgs = nixpkgs.legacyPackages.${system};
        in mkHost (import ./hosts/yinzhou { inherit system pkgs; });

        shuiku = let
          system = "x86_64-linux";
          pkgs = nixpkgs.legacyPackages.${system};
        in mkHost (import ./hosts/shuiku { inherit system pkgs; });
      };
    };
}
