{ pkgs, ... }: {
  programs.firefox.languagePacks = [ "zh-CN" ];
  services.xserver.enable = true;
  services.xserver.displayManager.autoLogin.user = "mima";
  services.xserver.desktopManager.lxqt.enable = true;
  i18n.defaultLocale = "zh_CN.UTF-8";
  i18n.inputMethod.enabled = "ibus";
  i18n.inputMethod.ibus.engines =
    builtins.attrValues { inherit (pkgs.ibus-engines) libpinyin; };
  users.users = {
    mima = {
      # "!" means login disabled
      initialHashedPassword =
        "$6$UxT9KYGGV6ik$BhH3Q.2F8x1llZQLUS1Gm4AxU7bmgZUP7pNX6Qt3qrdXUy7ZYByl5RVyKKMp/DuHZgk.RiiEXK8YVH.b2nuOO/";
      isNormalUser = true;
      group = "mima";
      extraGroups = [
        "wheel"
        # net
        "networkmanager"
      ];
    };
  };
}
