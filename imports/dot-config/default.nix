{ config, lib, pkgs, ... }:
let inherit (lib) mkDefault mkForce;
in {
  home-manager.users.yc = {
    home = {
      packages = builtins.attrValues {
        inherit (pkgs)
          mg shellcheck _7zz xournalpp
          # book scanning and pdf-tools support for emacs
          ffmpeg nixfmt qrencode jmtpfs gpxsee
          # pdf processor in Go
          pdfcpu
          # image editor
          nomacs
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
        size = 16;
      };
    };
    dconf.settings = {
      "org/gnome/desktop/interface" = {
        enable-animations = false;
        gtk-key-theme = "Emacs";
      };
    };

    qt = { enable = true; };

    wayland.windowManager.sway = {
      enable = true;
      # this package is installed by NixOS
      # not home-manager
      package = null;
      xwayland = false;
      systemd.enable = true;
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
          "${modifier}+o" = "exec emacsclient --create-frame";
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
        };
        seat = {
          "*" = {
            hide_cursor = "when-typing enable";
            xcursor_theme = "Adwaita 16";
          };
        };
        input = {
          "type:touchpad" = {
            tap = "enabled";
            natural_scroll = "enabled";
            middle_emulation = "enabled";
            scroll_method = "edge";
            pointer_accel = "0.05";
          };
          "1149:8257:Kensington_Kensington_Slimblade_Trackball" = {
            left_handed = "enabled";
            pointer_accel = "0.25";
          };
        };
        output = { "*" = { scale = mkDefault "2"; }; };
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
        "sway/yc-sticky-keymap".source = ./xdg-configFile/sway-yc-sticky-keymap;
        "yc.sh".source = ./xdg-configFile/bashrc-config.sh;
        "w3m/bookmark.html".source = ./xdg-configFile/w3m-bookmark.html;
        "xournalpp/settings.xml".source =
          ./xdg-configFile/xournalpp-settings.xml;
        "xournalpp/toolbar.ini".source = ./xdg-configFile/xournalpp-toolbar.ini;
        "gpxsee/gpxsee.conf".source = ./xdg-configFile/gpxsee.conf;
        "fuzzel/fuzzel.ini".text = ''
          [main]
          font=sans-serif:size=12:weight=bold
          dpi-aware=no
          [colors]
          background=ffffffff
        '';
        "nomacs/Image Lounge.conf".source = ./xdg-configFile/nomacs-settings.conf;
        "w3m/config".source = ./xdg-configFile/w3m-config;
        "w3m/keymap".source = ./xdg-configFile/w3m-keymap;
      };
    };
  };
}
