{
  services.xserver = {
    layout = "yc";
    extraLayouts."yc" = {
      description = "zfs-root layout";
      languages = [ "eng" ];
      symbolsFile = ./ergo-keymap-yc.txt;
    };
  };
  home-manager.users.yc.wayland.windowManager.sway.config.input."type:keyboard".xkb_file =
    "$HOME/.config/sway/yc-sticky-keymap";
}
