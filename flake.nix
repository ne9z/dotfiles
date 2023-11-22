{
  description = "Barebones NixOS on ZFS config";

  inputs = {
    # https://channels.nixos.org/nixpkgs-unstable/git-revision
    # https://channels.nixos.org/nixos-unstable/git-revision
    # using this one
    # https://channels.nixos.org/nixos-unstable-small/git-revision
    nixpkgs.url = "nixpkgs/37e6adc926fc74e270e86a544a41aecabb0effb1";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager }@inputs:
    let
      mkHost = hostName: system:
        nixpkgs.lib.nixosSystem {
          pkgs = import nixpkgs {
            inherit system;
            nixpkgs.pkgs.zathura.useMupdf = true;
            overlays = [
              (final: prev: rec {
                zathura_core = prev.zathuraPkgs.zathura_core.overrideAttrs
                  (o: { patches = [ ./zathura-restart_syscall.patch ]; });
                zathura = prev.zathuraPkgs.zathuraWrapper.override {
                  inherit zathura_core;
                };
              })
            ];
          };
          specialArgs = { inherit inputs; };

          modules = [
            # Root on ZFS related configuration
            ./modules

            # Configuration shared by all hosts
            ./configuration.nix

            # Configuration per host
            ./hosts/${hostName}

            # home-manager
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
            }
          ];
        };
    in {
      nixosConfigurations = {
        qinghe = mkHost "qinghe" "x86_64-linux";
        tieling = mkHost "tieling" "x86_64-linux";
        yinzhou = mkHost "yinzhou" "x86_64-linux";
        kaiyuan = mkHost "kaiyuan" "x86_64-linux";
        # nix build .#nixosConfigurations.ceshi.config.system.build.isoImage
        ceshi = mkHost "ceshi" "x86_64-linux";
      };
    };
}
