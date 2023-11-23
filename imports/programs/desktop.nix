{
  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = (if config.programs.sway.enable then "qt" else "tty");
    enableSSHSupport = true;
  };
  security.chromiumSuidSandbox.enable = true;
  programs.chromium = {
    enable = true;
    homepageLocation = "https://lite.duckduckgo.com/";
    extraOpts = {
      "BookmarkBarEnabled" = false;
      "DefaultFileSystemWriteGuardSetting" = 2;
      "DefaultFileSystemReadGuardSetting" = 2;
      "DefaultJavaScriptJitSetting" = 2;
      "AlwaysOpenPdfExternally" = true;
      "HttpsUpgradesEnabled" = true;
      "HighEfficiencyModeEnabled" = true;
      # set to int 2 to disable
      "ShowHomeButton" = true;
      "HomepageLocation" = "https://lite.duckduckgo.com/";
      "NewTabPageLocation" = "chrome://settings/content/javascript";
      "DefaultInsecureContentSetting" = 2;
      "DefaultNotificationsSetting" = 2;
      "DefaultWebUsbGuardSetting" = 2;
      "DefaultPopupsSetting" = 1;
      "PasswordManagerEnabled" = false;
    };
  };
  # sway and related sound config
  hardware.opengl.extraPackages = builtins.attrValues { inherit (pkgs) vaapiIntel intel-media-driver; };
  hardware.pulseaudio.enable = false;
  sound.enable = true;
  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };
  programs.sway = {
    extraSessionCommands = ''
      export MOZ_ENABLE_WAYLAND=1
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';
    enable = true;
    extraPackages = builtins.attrValues {
      inherit (pkgs)
        swaylock swayidle foot gammastep wl-gammactl brightnessctl fuzzel grim
        libva-utils w3m gsettings-desktop-schemas pavucontrol waybar
        wl-clipboard wf-recorder;
    };
    # must be enabled, or else many programs will crash
    wrapperFeatures.gtk = true;
  };
}
