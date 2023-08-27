{
  inputs = {
    # https://channels.nixos.org/nixpkgs-unstable/git-revision
    # https://channels.nixos.org/nixos-unstable/git-revision
    # using this one
    # https://channels.nixos.org/nixos-unstable-small/git-revision
    nixpkgs.url =
      "github:nixos/nixpkgs/98008a3c477a7ea470f89f0e3484b00211fffc30";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager }:
    let
      mkHost = hostName: system:
        (({ zfs-root, yc, pkgs, system, ... }:
          nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
              # Module 0: zfs-root and my-config
              ./modules

              # Module 1: host-specific config, if exist
              (if (builtins.pathExists
                ./hosts/${hostName}/configuration.nix) then
                (import ./hosts/${hostName}/configuration.nix { inherit pkgs; })
              else
                { })

              # Module 2: entry point
              (({ zfs-root, yc, pkgs, lib, ... }: {
                inherit zfs-root yc;
                system.configurationRevision = if (self ? rev) then
                  self.rev
                else
                  throw "refuse to build: git tree is dirty";
                system.stateVersion = "23.05";
                imports = [
                  "${nixpkgs}/nixos/modules/installer/scan/not-detected.nix"
                  "${nixpkgs}/nixos/modules/profiles/hardened.nix"
                ];
              }) {
                inherit zfs-root yc pkgs;
                lib = nixpkgs.lib;
              })

              # Module 3: home-manager
              home-manager.nixosModules.home-manager
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
              }
            ];
          })

        # configuration input
          (import ./hosts/${hostName} {
            system = system;
            pkgs = nixpkgs.legacyPackages.${system};
          }));
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
