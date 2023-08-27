{ ... }: {
  networking = {
    hostName = "qinghe";
    hostId = "abcd1234";
  };
  home-manager.users.yc.wayland.windowManager.sway.config.input = {
    "9580:110:PenTablet_Pen" = {
      # the ratio is 16:9 on the graphic tablet
      # on a 3840x2160 screen, map to upper-right
      # multiply by 170
      # parameters:
      # offset from upper left
      # map_to_region = "x_offset y_offset x_map_to y_map_to"
      map_to_region = "1120 0 2720 1530";
    };
    "1386:830:Wacom_Intuos_PT_M_2_Pen" = {
      map_to_region = "1120 0 2720 1530";
    };
  };
}
