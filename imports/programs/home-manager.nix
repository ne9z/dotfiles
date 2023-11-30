{ config, pkgs, lib, ... }:
let inherit (lib) mkDefault;
in {
  home-manager.users.yc.programs = {
    bash = {
      enable = true;
      initExtra = "if [ -f ~/.config/yc.sh ]; then source ~/.config/yc.sh; fi";
    };
    git = {
      enable = true;
      userEmail = "yguo@posteo.net";
      userName = "Yǔchēn Guō 郭宇琛";
    };
    ssh = {
      matchBlocks = {
        "unixpool.math.tu-berlin.de".user = "cma-1e1";
        "codeberg.org".user = "git";
        "github.com".user = "git";
        "tl.yc" = {
          user = "yc";
          port = 65222;
        };
        "3ldetowqifu5ox23snmoblv7xapkd26qyvex6fwrg6zpdwklcatq.b32.i2p" = {
          user = "yc";
          port = 65222;
          proxyCommand = "${pkgs.libressl.nc}/bin/nc -x localhost:4447 %h %p";
        };
        "ditgedyyvwsxspdmgpnzuzhj7p63snkiok54cphmvwcgnrjgw2lqgcad.onion" = {
          user = "yc";
          port = 22;
          proxyCommand = "${pkgs.libressl.nc}/bin/nc -x localhost:9050 %h %p";
        };
      };
    };
    home-manager.enable = true;
    # use gnus instead
    mbsync.enable = false;
    msmtp.enable = false;
    notmuch.enable = false;
    mpv = {
      enable = true;
      config = {
        hwdec = mkDefault "vaapi";
        player-operation-mode = "cplayer";
        audio-pitch-correction = "no";
        vo = "gpu-next";
        ao = "pipewire";
      };
    };
    yt-dlp = {
      enable = true;
      settings = { format-sort = "codec:h264"; };
    };
    zathura = {
      enable = true;
      options = {
        default-bg = "#FFFFFF";
        default-fg = "#000000";
        index-bg = "#FFFFFF";
        index-fg = "#000000";
        sandbox = "strict";
      };
      mappings = {
        "[index] <A-\\<>" = "navigate_index top";
        "[index] <A-\\>>" = "navigate_index bottom";
        "[index] <A-i>" = "navigate_index collapse-all";
        "[index] <A-s>" = "toggle_statusbar";
        "[index] <A-v>" = "navigate_index up";
        "[index] <BackSpace>" = "navigate_index up";
        "[index] <C-[>" = "toggle_index";
        "[index] <C-b>" = "navigate_index collapse";
        "[index] <C-c>" = "toggle_index";
        "[index] <C-f>" = "navigate_index expand";
        "[index] <C-g>" = "toggle_index";
        "[index] <C-h>" = "navigate_index up";
        "[index] <C-i>" = "navigate_index expand-all";
        "[index] <C-j>" = "navigate_index select";
        "[index] <C-m>" = "navigate_index select";
        "[index] <C-n>" = "navigate_index down";
        "[index] <C-p>" = "navigate_index up";
        "[index] <C-v>" = "navigate_index down";
        "[index] <Down>" = "navigate_index down";
        "[index] <Esc>" = "toggle_index";
        "[index] <Left>" = "navigate_index collapse";
        "[index] <Return>" = "navigate_index select";
        "[index] <Right>" = "navigate_index expand";
        "[index] <Space>" = "navigate_index select";
        "[index] <Up>" = "navigate_index up";
        "[index] \\<" = "navigate_index top";
        "[index] \\>" = "navigate_index bottom";
        "[index] i" = "toggle_index";
        "[index] q" = "toggle_index";
        "[fullscreen] +" = "zoom in";
        "[fullscreen] -" = "zoom out";
        "[fullscreen] <A-Space>" = "scroll full-up";
        "[fullscreen] <C-Space>" = "navigate next";
        "[fullscreen] <A-s>" = "toggle_statusbar";
        "[fullscreen] <BackSpace>" = "scroll full-up";
        "[fullscreen] <C-BackSpace>" = "navigate previous";
        "[fullscreen] a" = "scroll full-left";
        "[fullscreen] b" = "scroll left";
        "[fullscreen] e" = "scroll full-right";
        "[fullscreen] f" = "scroll right";
        "[fullscreen] i" = "toggle_index";
        "[fullscreen] n" = "scroll down";
        "[fullscreen] p" = "scroll up";
        "[fullscreen] <C-r>" = "search backward";
        "[fullscreen] <C-s>" = "search forward";
        "[fullscreen] <Down>" = "scroll down";
        "[fullscreen] <Left>" = "scroll left";
        "[fullscreen] <Return>" = "scroll full-down";
        "[fullscreen] <Right>" = "scroll right";
        "[fullscreen] <Space>" = "scroll full-down";
        "[fullscreen] <Tab>" = "toggle_index";
        "[fullscreen] <Up>" = "scroll up";
        "[fullscreen] =" = "zoom in";
        "[fullscreen] F" = "follow";
        "[fullscreen] \\'" = "mark_evaluate";
        "[fullscreen] \\," = "navigate next";
        "[fullscreen] \\." = "navigate previous";
        "[fullscreen] A" = "adjust_window best-fit";
        "[fullscreen] d" = "toggle_page_mode";
        "[fullscreen] l" = "jumplist backward";
        "[fullscreen] m" = "mark_add";
        "[fullscreen] q" = "quit";
        "[fullscreen] r" = "jumplist forward";
        "[fullscreen] s" = "adjust_window width";
        "[normal] +" = "zoom in";
        "[normal] -" = "zoom out";
        "[normal] <A-Space>" = "scroll full-up";
        "[normal] <C-Space>" = "navigate next";
        "[normal] <A-s>" = "toggle_statusbar";
        "[normal] <BackSpace>" = "scroll full-up";
        "[normal] <C-BackSpace>" = "navigate previous";
        "[normal] a" = "scroll full-left";
        "[normal] b" = "scroll left";
        "[normal] e" = "scroll full-right";
        "[normal] f" = "scroll right";
        "[normal] i" = "toggle_index";
        "[normal] n" = "scroll down";
        "[normal] p" = "scroll up";
        "[normal] <C-r>" = "search backward";
        "[normal] <C-s>" = "search forward";
        "[normal] <Down>" = "scroll down";
        "[normal] <Left>" = "scroll left";
        "[normal] <Return>" = "scroll full-down";
        "[normal] <Right>" = "scroll right";
        "[normal] <Space>" = "scroll full-down";
        "[normal] <Tab>" = "toggle_index";
        "[normal] <Up>" = "scroll up";
        "[normal] =" = "zoom in";
        "[normal] F" = "follow";
        "[normal] \\'" = "mark_evaluate";
        "[normal] \\," = "navigate next";
        "[normal] \\." = "navigate previous";
        "[normal] A" = "adjust_window best-fit";
        "[normal] d" = "toggle_page_mode";
        "[normal] l" = "jumplist backward";
        "[normal] m" = "mark_add";
        "[normal] q" = "quit";
        "[normal] R" = "rotate";
        "[normal] r" = "jumplist forward";
        "[normal] s" = "adjust_window width";
      };
      options = {
        selection-clipboard = "clipboard";
        window-title-basename = true;
        adjust-open = "width";
        scroll-page-aware = false;
        scroll-full-overlap = "0.1";
        statusbar-home-tilde = true;
        synctex = true;
        font = "sans-serif bold 10";
        guioptions = "";
        zoom-step = 9;
        scroll-step = 80;
        scroll-hstep = 80;
      };
    };
    ssh = {
      enable = true;
      hashKnownHosts = true;
    };
    foot = {
      enable = true;
      server.enable = true;
      settings = {
        main = {
          term = "foot-direct";
          dpi-aware = "no";
          font = mkDefault "monospace:size=10";
        };

        url = { launch = "wl-copy \${url}"; };
        cursor = { color = "ffffff 000000"; };
        colors = {
          # https://codeberg.org/dnkl/foot/raw/branch/master/themes/modus-operandi
          background = "ffffff";
          foreground = "000000";
          regular0 = "000000";
          regular1 = "a60000";
          regular2 = "005e00";
          regular3 = "813e00";
          regular4 = "0031a9";
          regular5 = "721045";
          regular6 = "00538b";
          regular7 = "bfbfbf";
          bright0 = "595959";
          bright1 = "972500";
          bright2 = "315b00";
          bright3 = "70480f";
          bright4 = "2544bb";
          bright5 = "5317ac";
          bright6 = "005a5f";
          bright7 = "ffffff";
        };
      };
    };
    chromium = {
      enable = mkDefault true;
      package = pkgs.ungoogled-chromium;
      # https://www.chromium.org/administrators/
      # https://github.com/gorhill/uBlock/wiki/Deploying-uBlock-Origin
      commandLineArgs = [
        "--ozone-platform-hint=auto"
        "--ignore-gpu-blocklist"
        "--js-flags='--jitless'"
        "--enable-vulkan"
        "--enable-features=VaapiVideoEncoder,VaapiVideoDecoder,CanvasOopRasterization,TouchpadOverscrollHistoryNavigation"
        "--enable-gpu-rasterization"
        "--enable-zero-copy"
        "--start-maximized"
        "--disable-features=UseChromeOSDirectVideoDecoder"
        "--disable-remote-fonts"
        "--disable-webgl"
        "--hide-fullscreen-exit-ui"
        "--remove-grab-handle"
        "--force-punycode-hostnames"
        "--popups-to-tabs"
      ];
    };
    password-store = {
      enable = true;
      package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);
      settings = {
        PASSWORD_STORE_GENERATED_LENGTH = "32";
        PASSWORD_STORE_CHARACTER_SET = "[:alnum:].,";
        PASSWORD_STORE_DIR = "$HOME/.password-store";
      };
    };
    waybar = {
      enable = true;
      style = ./waybar-style.css;
      systemd = {
        enable = true;
        target = "sway-session.target";
      };
      settings = {
        mainBar = {
          id = "bar-0";
          idle_inhibitor = {
            format = "{icon}";
            format-icons = {
              activated = "NOLOCK";
              deactivated = "LOCK";
            };
          };
          ipc = true;
          layer = "bottom";
          modules-center = [ ];
          modules-left = [ "sway/workspaces" "sway/mode" ];
          modules-right =
            [ "idle_inhibitor" "pulseaudio" "backlight" "battery" "clock" ];
          position = "bottom";
          pulseaudio = {
            format = "VOL {volume}%";
            format-muted = "MUTED";
            on-click = "${pkgs.alsa-utils}/bin/amixer set Master toggle";
          };
          backlight = {
            device = "intel_backlight";
            format = "BRI {percent}%";
            on-click-right = "${pkgs.brightnessctl}/bin/brightnessctl set 100%";
            on-click = "${pkgs.brightnessctl}/bin/brightnessctl set 1%";
            on-scroll-down = "${pkgs.brightnessctl}/bin/brightnessctl set 1%-";
            on-scroll-up = "${pkgs.brightnessctl}/bin/brightnessctl set +1%";
          };
          "wlr/taskbar" = {
            active-first = true;
            format = "{name}";
            on-click = "activate";
            all-outputs = false;
          };
          battery = { format = "BAT {capacity}%"; };
          clock = { format-alt = "{:%a, %d. %b  %H:%M}"; };
          "sway/workspaces" = {
            disable-scroll-wraparound = true;
            enable-bar-scroll = true;
          };

        };
      };
    };

    swaylock.settings = {
      color = "808080";
      indicator-idle-visible = true;
      indicator-radius = 100;
      line-color = "ffffff";
      show-failed-attempts = true;
    };
  };
}
