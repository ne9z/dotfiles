{ ... }:
let user-name = "yc";
in {
  imports =
    [ ../../imports/common.nix ../../imports/desktop.nix ../../imports/yc.nix ];
  networking = {
    hostName = "qinghe";
    hostId = "abcd1234";
  };
  time.timeZone = "Europe/Berlin";
  environment.sessionVariables = {
    QT_WAYLAND_FORCE_DPI = "288";
    GDK_DPI_SCALE = "3";
  };

  home-manager.users.yc = {
    programs.foot.settings.main.font = "monospace:size=16";
    programs.mpv.config.hwdec = "no";
    wayland.windowManager.sway.config = {
      input = {
        "9580:110:PenTablet_Pen" = {
          # the ratio is 16:9 on the graphic tablet
          # on a 3840x2160 screen, scaled with 3 is
          # 1280x720 on the whole screen
          # map to upper-right
          # multiply by 170
          # parameters:
          # offset from upper left
          # map_to_region = "x_offset y_offset x_map_to y_map_to"
          # tablet:whole ratio = 9:16
          # active area: 20×13 cm
          map_to_region = "1840 860 2000 1300";
        };
        "1386:830:Wacom_Intuos_PT_M_2_Pen" = {
          # tablet:whole ratio = 9:16
          # active area: 216x135mm
          # formula: 135x720/216
          map_to_region = "1680 810 2160 1350";
        };
        "9580:109:GAOMON_Gaomon_Tablet_M10KPRO_Pen" = {
          # active area: 255 x 159.8mm
          # tablet:whole ratio = 9:16
          map_to_region = "1300 572 2540 1588";
        };
        "9580:109:GAOMON_Gaomon_Tablet_Pen" = {
          # active area: 254 x 158.8mm
          # tablet:whole ratio = 9:16
          map_to_region = "1300 572 2540 1588";
        };
      };
    };
  };
}
