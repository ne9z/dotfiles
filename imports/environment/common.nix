{ pkgs, ... }: {
  environment = {
    systemPackages = builtins.attrValues {
      inherit (pkgs)
        mg # emacs-like editor
        jq # other programs
      ;
      # By default, the system will only use packages from the
      # stable channel. i.e.
      # inherit (pkg) my-favorite-stable-package;
      # You can selectively install packages
      # from the unstable channel. Such as
      # inherit (pkgs-unstable) my-favorite-unstable-package;
      # You can also add more
      # channels to pin package version.
    };
    etc."wpa_supplicant.conf".text = "#";
    memoryAllocator.provider = "mimalloc";
  };
}
