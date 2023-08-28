{ ... }:
{
  services.xserver.desktopManager.lxqt.enable = true;
  i18n.defaultLocale = "zh_CN.UTF-8";
  i18n.inputMethod.enabled = "fcitx5";
  users.mutableUsers = false;
  users.users = {
    gg = {
      # "!" means login disabled
      initialHashedPassword = "$6$UxT9KYGGV6ik$BhH3Q.2F8x1llZQLUS1Gm4AxU7bmgZUP7pNX6Qt3qrdXUy7ZYByl5RVyKKMp/DuHZgk.RiiEXK8YVH.b2nuOO/";
      isNormalUser = true;
    };
  };
}
