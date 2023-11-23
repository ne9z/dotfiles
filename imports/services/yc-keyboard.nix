{
  services.xserver = {
    layout = "yc";
    extraLayouts."yc" = {
      description = "zfs-root layout";
      languages = [ "eng" ];
      symbolsFile = ./ergo-keymap-yc.txt;
    };
  };
}
