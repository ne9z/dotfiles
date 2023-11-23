{ config, ... }: {
  users.users = {
    yc = {
      # "!" means login disabled
      initialHashedPassword =
        "$6$UxT9KYGGV6ik$BhH3Q.2F8x1llZQLUS1Gm4AxU7bmgZUP7pNX6Qt3qrdXUy7ZYByl5RVyKKMp/DuHZgk.RiiEXK8YVH.b2nuOO/";
      description = "Yǔchēn Guō 郭宇琛";
      # a default group must be set
      extraGroups = [
        # use doas
        "wheel"
        # manage VMs
        "libvirtd"
      ];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICkDT9xZLh+lHc6Z60oLZlLjzOcP39B3D7ptV6xSzAhu openpgp:0x464B6BB1"
      ];
      isNormalUser = true;
      uid = 1000;
    };
  };
  home-manager.users.yc.home = {
    username = "yc";
    homeDirectory = "/home/yc";
    stateVersion = config.system.stateVersion;
  };

}
