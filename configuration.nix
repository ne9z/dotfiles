{ zfs-root, inputs, pkgs, lib, ... }: {
  # load module config to here
  inherit zfs-root;

  # Let 'nixos-version --json' know about the Git revision
  # of this flake.
  system.configurationRevision = if (inputs.self ? rev) then
    inputs.self.rev
  else
    throw "refuse to build: git tree is dirty";

  system.stateVersion = "23.05";

  nixpkgs.config.zathura.useMupdf = true;

  # Enable NetworkManager for wireless networking,
  # You can configure networking with "nmtui" command.
  networking.useDHCP = lib.mkDefault true;
  networking.networkmanager.enable = lib.mkDefault false;

  # Enable GNOME
  # GNOME must be used with a normal user account.
  # However, by default, only root user is configured.
  # Create a normal user and set password in
  # hosts/exampleHost/default.nix
  #
  # You need to enable all options in this attribute set.
  services.xserver = {
    enable = false;
    desktopManager.gnome.enable = false;
    displayManager.gdm.enable = false;
  };

  # Enable Sway window manager
  # Sway must be used with a normal user account.
  # However, by default, only root user is configured.
  # Create a normal user and set password in
  # hosts/exampleHost/default.nix
  programs.sway.enable = lib.mkDefault false;

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
  };

  imports = [
    "${inputs.nixpkgs}/nixos/modules/installer/scan/not-detected.nix"
    "${inputs.nixpkgs}/nixos/modules/profiles/hardened.nix"
  ];

  services.openssh = {
    enable = lib.mkDefault true;
    settings = { PasswordAuthentication = lib.mkDefault false; };
  };

  boot.zfs.forceImportRoot = lib.mkDefault false;

  nix.settings.experimental-features = lib.mkDefault [ "nix-command" "flakes" ];

  programs.git.enable = true;

  services.logrotate.checkConfig = false;
  security = {
    doas.enable = lib.mkDefault true;
    sudo.enable = lib.mkDefault false;
  };

  environment.systemPackages = builtins.attrValues {
    inherit (pkgs)
      mg # emacs-like editor
      parted # other programs
    ;
  };

  security = {
    allowSimultaneousMultithreading = false;
    lockKernelModules = false;
    apparmor.enable = true;
  };
  environment.memoryAllocator.provider = "libc";

  nix = {
    gc = {
      automatic = true;
      options = "--delete-old";
    };
  };
}
