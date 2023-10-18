{ config, lib, pkgs, ... }:
let
  inherit (lib) mkDefault;
  emacsPkg = import ./emacs.nix { inherit pkgs; };
in {
  programs.home-manager.enable = true;
  home = {
    packages = builtins.attrValues {
      inherit (pkgs)
        mg shellcheck _7zz xournalpp
        # book scanning and pdf-tools support for emacs
        poppler_utils libtiff scantailor-advanced ffmpeg nixfmt qrencode
        zathura jmtpfs gpxsee
        # pdf processor in Go
        pdfcpu
        # image editor
        nomacs
        # CoMa programs
        python3
        # pdf manipulation suite in C++
        # https://qpdf.readthedocs.io/en/stable/
        qpdf;
    };
  };
  gtk = {
    enable = true;
    theme = {
      name = "Adwaita";
      package = pkgs.gnome.gnome-themes-extra;
    };
    iconTheme = {
      package = pkgs.gnome.adwaita-icon-theme;
      name = "Adwaita";
    };
    cursorTheme = {
      name = "Adwaita";
      size = 48;
    };
  };
  dconf.settings = {
    "org/gnome/desktop/interface" = {
      enable-animations = false;
      gtk-key-theme = "Emacs";
    };
  };
  services.emacs = {
    enable = true;
    package = emacsPkg;
    client.enable = true;
    client.arguments = [ "--create-frame" ];
    defaultEditor = true;
    startWithUserSession = "graphical";
  };
  programs = {
    git = {
      enable = true;
      userEmail = mkDefault "me@example.net";
      userName = mkDefault "赵本山";
    };
    # use gnus instead
    mbsync.enable = false;
    msmtp.enable = false;
    notmuch.enable = false;
  };
  programs.mpv = {
    enable = true;
    config = {
      hwdec = mkDefault "vaapi";
      player-operation-mode = "cplayer";
      audio-pitch-correction = "no";
      vo = "gpu-next";
      ao = "pipewire";
    };
  };
  programs.yt-dlp = {
    enable = true;
    settings = { format-sort = "codec:h264"; };
  };
  programs.zathura = {
    enable = true;
    options = {
      default-bg = "#FFFFFF";
      default-fg = "#000000";
      index-bg = "#FFFFFF";
      index-fg = "#000000";
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
  programs.ssh = {
    enable = true;
    hashKnownHosts = true;
  };
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        term = "foot-direct";
        dpi-aware = "yes";
        font = mkDefault "monospace:size=12";
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
  programs.chromium = {
    enable = mkDefault false;
    package = pkgs.ungoogled-chromium;
    # https://www.chromium.org/administrators/
    # https://github.com/gorhill/uBlock/wiki/Deploying-uBlock-Origin
    commandLineArgs = [
      "--incognito"
      "--ozone-platform-hint=auto"
      "--ignore-gpu-blocklist"
      "--disable-remote-fonts"
      "--js-flags='--jitless'"
      "--enable-features=TouchpadOverscrollHistoryNavigation"
      "--start-maximized"
    ];
  };
  qt = {
    enable = true;
    platformTheme = "gnome";
    style = {
      package = pkgs.adwaita-qt;
      name = "adwaita";
    };
  };
  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);
    settings = {
      PASSWORD_STORE_GENERATED_LENGTH = "32";
      PASSWORD_STORE_CHARACTER_SET = "[:alnum:].,";
      PASSWORD_STORE_DIR = "$HOME/.password-store";
    };
  };
  programs.waybar = {
    enable = true;
    style = ./not-nix-config-files/waybar-style.css;
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
  services.swayidle = {
    enable = true;
    events = [
      {
        event = "before-sleep";
        command = "${pkgs.swaylock}/bin/swaylock";
      }
      {
        event = "lock";
        command = "lock";
      }
    ];
    timeouts = [
      {
        timeout = 900;
        command = "${pkgs.swaylock}/bin/swaylock -fF";
      }
      {
        timeout = 910;
        command = "${pkgs.systemd}/bin/systemctl suspend";
      }
    ];
  };
  programs.swaylock.settings = {
    color = "808080";
    indicator-idle-visible = true;
    indicator-radius = 100;
    line-color = "ffffff";
    show-failed-attempts = true;
  };
  wayland.windowManager.sway = {
    enable = true;
    # this package is installed by NixOS
    # not home-manager
    package = null;
    xwayland = false;
    systemd.enable = true;
    extraConfig = ''
      mode "default" {
       bindsym --no-warn Mod4+Backspace focus mode_toggle
       bindsym --no-warn Mod4+Control+Shift+Space move left
       bindsym --no-warn Mod4+Control+Space move right
       bindsym --no-warn Mod4+Control+b move left
       bindsym --no-warn Mod4+Control+e focus parent
       bindsym --no-warn Mod4+Control+f move right
       bindsym --no-warn Mod4+Control+n move down
       bindsym --no-warn Mod4+Control+p move up
       bindsym --no-warn Mod4+Shift+Backspace floating toggle
       bindsym --no-warn Mod4+Shift+Space focus left
       bindsym --no-warn Mod4+Space focus right
       bindsym --no-warn Mod4+b focus left
       bindsym --no-warn Mod4+g focus parent; focus left; focus child
       bindsym --no-warn Mod4+e focus parent; focus right; focus child
       bindsym --no-warn Mod4+f focus right
       bindsym --no-warn Mod4+f11 fullscreen
       bindsym --no-warn Mod4+k kill
       bindsym --no-warn Mod4+n focus down
       bindsym --no-warn Mod4+o workspace next
       bindsym --no-warn Mod4+p focus up
       bindsym --no-warn Mod4+w move scratchpad
       bindsym --no-warn Mod4+x workspace back_and_forth
       bindsym --no-warn Mod4+y scratchpad show
       bindsym --no-warn Shift+Print exec ${pkgs.grim}/bin/grim
       bindsym --no-warn Mod4+Shift+l exec ${pkgs.systemd}/bin/systemctl suspend
       bindsym --no-warn Mod4+o exec ${emacsPkg}/bin/emacsclient --create-frame
       bindsym --no-warn Mod4+t layout tabbed
      }

      mode "resize" {
       bindsym --no-warn b resize shrink width 10px
       bindsym --no-warn f resize grow width 10px
       bindsym --no-warn n resize grow height 10px
       bindsym --no-warn p resize shrink height 10px
       bindsym --no-warn Space mode default
      }
      titlebar_padding 1
      titlebar_border_thickness 0
    '';
    config = {
      colors = {
        background = "#ffffff";
        focused = {
          background = "#8fffff";
          border = "#8fffff";
          text = "#000000";
          childBorder = "#285577";
          indicator = "#2e9ef4";

        };
        unfocused = {
          background = "#fafafa";
          border = "#fafafa";
          text = "#000000";
          childBorder = "#5f676a";
          indicator = "#484e50";
        };
        focusedInactive = {
          background = "#fafafa";
          border = "#fafafa";
          text = "#000000";
          childBorder = "#222222";
          indicator = "#292d2e";
        };
      };
      fonts = {
        names = [ "sans-serif" ];
        style = "bold";
        size = mkDefault 16.0;
      };
      modes = {
        default = { };
        resize = { };
      };
      seat = {
        "*" = {
          hide_cursor = "when-typing enable";
          xcursor_theme = "Adwaita 48";
        };
      };
      input = {
        "type:touchpad" = {
          tap = "enabled";
          natural_scroll = "enabled";
          middle_emulation = "enabled";
          scroll_method = "edge";
          pointer_accel = "0.3";
        };
        "1149:8257:Kensington_Kensington_Slimblade_Trackball" = {
          left_handed = "enabled";
          pointer_accel = "1";
        };
      };
      output = {
        "*" = {
          background =
            "${pkgs.sway}/share/backgrounds/sway/Sway_Wallpaper_Blue_1920x1080.png fill";
        };
      };
      modifier = "Mod4";
      menu = "${pkgs.fuzzel}/bin/fuzzel";
      startup = [{
        command = "${pkgs.systemd}/bin/systemctl --user restart waybar";
        always = true;
      }];
      terminal = "${pkgs.foot}/bin/foot ${pkgs.tmux}/bin/tmux attach-session";
      window = {
        hideEdgeBorders = "smart";
        titlebar = true;
      };
      workspaceAutoBackAndForth = true;
      workspaceLayout = "tabbed";
      focus = {
        followMouse = "always";
        wrapping = "workspace";
      };
      gaps = {
        smartBorders = "no_gaps";
        smartGaps = true;
      };
      bars = [{
        command = "${pkgs.waybar}/bin/waybar";
        id = "bar-0";
        mode = "hide";
        position = "bottom";
      }];
      floating = { titlebar = true; };
    };
  };
  xdg = {
    mimeApps = {
      enable = true;
      defaultApplications = {
        "application/pdf" = "org.pwmt.zathura.desktop";
        "image/jpeg" = "org.nomacs.ImageLounge.desktop";
        "image/tiff" = "org.nomacs.ImageLounge.desktop";
        "application/vnd.comicbook+zip" = "org.pwmt.zathura.desktop";
      };
    };
    configFile = {
      "xournalpp/settings.xml" = {
        source = ./not-nix-config-files/xournalpp-settings.xml;
      };
      "xournalpp/toolbar.ini" = {
        source = ./not-nix-config-files/xournalpp-toolbar.ini;
      };
      "gpxsee/gpxsee.conf" = { source = ./not-nix-config-files/gpxsee.conf; };
      "latexmk/latexmkrc" = { text = ''$pdf_previewer = "zathura"''; };
      "emacs/init.el" = { source = ./not-nix-config-files/emacs-init.el; };
      "nomacs/Image Lounge.conf" = {
        source = ./not-nix-config-files/nomacs-config.conf;
      };
      "scantailor-advanced/scantailor-advanced.ini" = {
        source = ./not-nix-config-files/scantailor-advanced.ini;
      };
      "fuzzel/fuzzel.ini" = {
        text = ''
          [main]
          font=sans-serif:size=14
          dpi-aware=no'';
      };
      "w3m/config" = { source = ./not-nix-config-files/w3m-config; };
      "w3m/keymap" = { source = ./not-nix-config-files/w3m-keymap; };
    };
  };
}
