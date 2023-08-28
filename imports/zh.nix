{ pkgs, ... }:
{
  environment = {
    systemPackages = builtins.attrValues {
      inherit (pkgs) firefox-esr ungoogled-chromium mg;
    };
  };
  services.xserver.enable = true;
  services.xserver.displayManager = {
    autoLogin.user = "yc";
    lightdm.enable = true;
  };
  services.xserver.desktopManager.lxqt.enable = true;
  i18n.defaultLocale = "zh_CN.UTF-8";
  i18n.inputMethod.enabled = "ibus";
  i18n.inputMethod.ibus.engines = builtins.attrValues {
    inherit (pkgs.ibus-engines) libpinyin;
  };
  users.mutableUsers = false;
  users.users = {
    mima = {
      # "!" means login disabled
      initialHashedPassword = "$6$UxT9KYGGV6ik$BhH3Q.2F8x1llZQLUS1Gm4AxU7bmgZUP7pNX6Qt3qrdXUy7ZYByl5RVyKKMp/DuHZgk.RiiEXK8YVH.b2nuOO/";
      isNormalUser = true;
      group = "mima";
      extraGroups = [ "wheel" ];
    };
  };
  fonts.fontconfig = {
    # disable bitmap unifont
    localConf = ''
      <selectfont>
        <rejectfont>
          <pattern>
             <patelt name="family" >
                <string>Unifont</string>
              </patelt>
          </pattern>
        </rejectfont>
      </selectfont>
    '';
    defaultFonts = {
      monospace = [ "DejaVu Sans Mono" "Source Han Mono SC" ];
      sansSerif = [ "DejaVu Sans" "Source Han Sans SC" ];
      serif = [ "DejaVu Serif" "Source Han Serif SC" ];
    };
  };
  fonts.packages = builtins.attrValues {
    inherit (pkgs)
      dejavu_fonts stix-two source-han-sans source-han-mono source-han-serif;
  };
}
