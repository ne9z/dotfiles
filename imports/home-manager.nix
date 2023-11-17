{ config, lib, pkgs, ... }:
let
  inherit (lib) mkDefault mkForce;
  emacsPkg = import ./emacs.nix { inherit pkgs; };
  mypython =
    pkgs.python3.withPackages (ps: builtins.attrValues { inherit (ps) sympy; });
in {
  programs.home-manager.enable = true;
  home = {
    packages = builtins.attrValues {
      inherit (pkgs)
        mg shellcheck _7zz xournalpp
        # book scanning and pdf-tools support for emacs
        ffmpeg nixfmt qrencode jmtpfs gpxsee
        # pdf processor in Go
        pdfcpu
        # image editor
        shotwell
        # pdf manipulation suite in C++
        # https://qpdf.readthedocs.io/en/stable/
        qpdf;
      inherit mypython;
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
    systemdIntegration = true;
    extraConfig = ''
      titlebar_padding 1
      titlebar_border_thickness 0
    '';
    config = {
      modes = {
        resize = {
          b = "resize shrink width 10px";
          f = "resize grow width 10px";
          n = "resize grow height 10px";
          p = "resize shrink height 10px";
          Space = "mode default";
          Escape = "mode default";
        };
      };
      keybindings = let modifier = "Mod4";
      in lib.mkOptionDefault {
        "${modifier}+k" = "kill";
        "${modifier}+b" = "focus left";
        "${modifier}+f" = "focus right";
        "${modifier}+n" = "focus down";
        "${modifier}+p" = "focus up";
        "${modifier}+c" = "focus child";
        "${modifier}+e" = "focus parent";
        "${modifier}+Control+b" = "focus output left";
        "${modifier}+Control+f" = "focus output right";
        "${modifier}+Control+n" = "focus output down";
        "${modifier}+Control+p" = "focus output up";
        "${modifier}+Control+t" = "focus tiling";
        "${modifier}+Control+l" = "focus floating";
        "${modifier}+Backspace" = "focus mode_toggle";
        "${modifier}+f11" = "fullscreen toggle";
        "${modifier}+t" = "layout toggle splitv splith tabbed";
        "${modifier}+Shift+b" = "move left";
        "${modifier}+Shift+f" = "move right";
        "${modifier}+Shift+n" = "move down";
        "${modifier}+Shift+p" = "move up";
        "${modifier}+w" = "move scratchpad";
        "${modifier}+y" = "scratchpad show";
        "${modifier}+x" = "workspace back_and_forth";
        "${modifier}+Shift+x" = "move workspace back_and_forth";
        "${modifier}+Control+Shift+b" = "move output left";
        "${modifier}+Control+Shift+f" = "move output right";
        "${modifier}+Control+Shift+n" = "move output down";
        "${modifier}+Control+Shift+p" = "move output up";
        "${modifier}+Control+Backspace" = "floating toggle";
        "${modifier}+space" = "focus right";
        "${modifier}+Shift+space" = "focus parent; focus right; focus child";
        "Shift+Print" = "exec ${pkgs.grim}/bin/grim";
        "${modifier}+Shift+l" = "exec ${pkgs.systemd}/bin/systemctl suspend";
        "${modifier}+o" = "exec ${emacsPkg}/bin/emacsclient --create-frame";
      };
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
      modifier = "Mod4";
      menu = "${pkgs.fuzzel}/bin/fuzzel";
      startup = [{
        command = "${pkgs.systemd}/bin/systemctl --user restart waybar";
        always = true;
      }];
      terminal =
        "${pkgs.foot}/bin/footclient ${pkgs.tmux}/bin/tmux attach-session";
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
        "image/jpeg" = "org.gnome.Shotwell";
        "image/tiff" = "org.gnome.Shotwell";
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
      "emacs/init.el" = { source = ./not-nix-config-files/emacs-init.el; };
      "fuzzel/fuzzel.ini" = {
        text = ''
          [main]
          font=sans-serif:size=14:weight=bold
          dpi-aware=yes'';
      };
      "w3m/config" = { source = ./not-nix-config-files/w3m-config; };
      "w3m/keymap" = { source = ./not-nix-config-files/w3m-keymap; };
    };
  };
}
