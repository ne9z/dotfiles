{ ... }: {
  imports = [ ../../imports/profiles/desktop.nix ];
  networking = {
    hostName = "qinghe";
    hostId = "abcd1234";
  };
  time.timeZone = "Europe/Berlin";
  # disable the deprecated radeon driver and force enable newer amdgpu driver
  boot.kernelParams = [
    "radeon.cik_support=0"
    "radeon.si_support=0"
    "amdgpu.cik_support=1"
    "amdgpu.si_support=1"
    "amdgpu.dc=1"
  ];
  boot.blacklistedKernelModules = [ "radeon" ];
  home-manager.users.yc = {
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
          map_to_region = "560 252 720 468";
        };
        "1386:830:Wacom_Intuos_PT_M_2_Pen" = {
          # tablet:whole ratio = 9:16
          # active area: 216x135mm
          # formula: 135x720/216
          map_to_region = "560 270 720 450";
        };
        "9580:109:GAOMON_Gaomon_Tablet_M10KPRO_Pen" = {
          # active area: 254 x 158.8mm
          # tablet:whole ratio = 9:16
          map_to_region = "560 270 720 450";
        };
        "9580:109:GAOMON_Gaomon_Tablet_Pen" = {
          # active area: 254 x 158.8mm
          # tablet:whole ratio = 9:16
          map_to_region = "560 270 720 450";
        };
      };
    };
  };
}
