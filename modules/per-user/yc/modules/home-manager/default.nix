{ config, lib, pkgs, ... }:
let
  cfg = config.zfs-root.per-user.yc.modules.home-manager;
  inherit (lib) mkDefault mkOption types mkIf recursiveUpdate;
in {
  options.zfs-root.per-user.yc.modules.home-manager = {
    enable = mkOption {
      type = types.bool;
      default = config.zfs-root.per-user.yc.enable;
    };
  };
  config = mkIf cfg.enable {
    # have a clean home
    zfs-root.fileSystems.datasets = { "rpool/nixos/home" = "/oldroot/home"; };
    fileSystems."/home/yc" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "rw" "size=1G" "uid=yc" "gid=users" "mode=1700" ];
    };
    programs.gnupg.agent = {
      enable = true;
      pinentryFlavor = (if config.programs.sway.enable then "qt" else "tty");
      enableSSHSupport = true;
    };
    services.tlp = {
      enable = true;
      settings = {
        BAY_POWEROFF_ON_BAT = "1";
        STOP_CHARGE_THRESH_BAT0 = "85";
      };
    };
    home-manager.users.yc = {
      home.packages =
        builtins.attrValues { inherit (pkgs) mg shellcheck _7zz; };

      gtk = {
        enable = true;
        font = { name = "sans-serif 16"; };
        theme = {
          name = "Adwaita";
          package = pkgs.gnome.gnome-themes-extra;
        };
        iconTheme = {
          package = pkgs.gnome.adwaita-icon-theme;
          name = "Adwaita";
        };
      };
      dconf.settings = {
        "org/gnome/desktop/interface" = {
          enable-animations = false;
          document-font-name = "sans-serif 16";
          monospace-font-name = "monospace 14";
          gtk-key-theme = "Emacs";
          cursor-size = 48;
        };
      };
      programs.bash = {
        enable = true;
        initExtra =
          "if [ -f ~/.config/yc.sh ]; then source ~/.config/yc.sh; fi";
      };
      programs = {
        git = {
          enable = true;
          userEmail = "yguo@posteo.net";
          userName = "Yuchen Guo";
        };
        mbsync.enable = true;
        msmtp.enable = true;
        notmuch.enable = true;
      };
      accounts.email = {
        maildirBasePath = "Maildir"; # relative to user home
        accounts = {
          "posteo" = {
            aliases = [ ];
            address = "yguo@posteo.net";
            passwordCommand = "pass show email/posteo | head -n1";
            primary = true;
            userName = "yguo@posteo.net";
            realName = "Yuchen Guo";
            imap = {
              host = "posteo.de";
              port = 993;
            };
            smtp = {
              host = "posteo.de";
              port = 465;
            };
            mbsync = {
              enable = true;
              create = "both";
              remove = "both";
              expunge = "both";
              flatten = ".";
            };
            msmtp = {
              enable = true;
              extraConfig = { auth = "plain"; };
            };
            notmuch.enable = true;
            gpg = {
              key = "yguo@posteo.net";
              encryptByDefault = false;
              signByDefault = true;
            };
          };
        };
      };
      programs.mpv = {
        enable = true;
        config = recursiveUpdate {
          hwdec = "vaapi";
          player-operation-mode = "cplayer";
          audio-pitch-correction = "no";
          vo = "dmabuf-wayland";
          ao = "pipewire";
        } (if (config.zfs-root.networking.hostName == "qinghe") then {
          hwdec = "no";
          vo = "gpu-next";
        } else
          { });
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
        };
        mappings = {
          "[normal] <C-b>" = "scroll left";
          "[normal] <C-n>" = "scroll down";
          "[normal] <C-p>" = "scroll up";
          "[normal] <C-f>" = "scroll right";
          "[normal] <C-g>" = "abort";
          "[insert] <C-g>" = "abort";
          "[normal] <C-[>" = "abort";
          "[normal] <A-\\<>" = "goto top";
          "[normal] <A-\\>>" = "goto bottom";
          "[normal] a" = "adjust_window best-fit";
          "[normal] s" = "adjust_window width";
          "[normal] F" = "display_link";
          "[normal] <C-c>" = "copy_link";
          "[normal] f" = "follow";
          "[normal] m" = "mark_add";
          "[normal] \\'" = "mark_evaluate";
          "[normal] \\," = "navigate next";
          "[normal] \\." = "navigate previous";
          "[normal] <A-Right>" = "navigate next";
          "[normal] <A-Left>" = "navigate previous";
          "[normal] <C-P>" = "print";
          "[normal] c" = "recolor";
          "[normal] R" = "reload";
          "[normal] v" = "rotate rotate_cw";
          "[normal] V" = "rotate rotate_ccw";
          "[normal] <Left>" = "scroll left";
          "[normal] <Up>" = "scroll up";
          "[normal] <Down>" = "scroll down";
          "[normal] <Right>" = "scroll right";
          "[normal] <A-a>" = "scroll half-left";
          "[normal] <C-V>" = "scroll half-down";
          "[normal] <A-V>" = "scroll half-up";
          "[normal] <A-e>" = "scroll half-right";
          "[normal] <C-a>" = "scroll full-left";
          "[normal] <C-v>" = "scroll full-down";
          "[normal] <Return>" = "scroll full-down";
          "[normal] <A-v>" = "scroll full-up";
          "[normal] <C-e>" = "scroll full-right";
          "[normal] <Space>" = "scroll full-down";
          "[normal] <A-Space>" = "scroll full-up";
          "[normal] <C-h>" = "scroll full-up";
          "[normal] <BackSpace>" = "scroll full-up";
          "[normal] <S-Space>" = "scroll full-up";
          "[normal] l" = "jumplist backward";
          "[normal] r" = "jumplist forward";
          "[normal] <A-r>" = "bisect forward";
          "[normal] <A-l>" = "bisect backward";
          "[normal] <C-s>" = "search forward";
          "[normal] <C-r>" = "search backward";
          "[normal] p" = "snap_to_page";
          "[normal] <C-i>" = "toggle_index";
          "[normal] i" = "toggle_index";
          "[normal] <Tab>" = "toggle_index";
          "[normal] <A-s>" = "toggle_statusbar";
          "[normal] <A-i>" = "focus_inputbar";
          "[normal] d" = "toggle_page_mode";
          "[normal] q" = "quit";
          "[normal] +" = "zoom in";
          "[normal] -" = "zoom out";
          "[normal] <C-+>" = "zoom in";
          "[normal] <C-->" = "zoom out";
          "[normal] =" = "zoom in";
          "[normal] <A-P>" = "toggle_presentation";
          "[normal] <A-F>" = "toggle_fullscreen";
          "[normal] j" = "toggle_fullscreen";
          "[fullscreen] j" = "toggle_fullscreen";
          "[fullscreen] q" = "toggle_fullscreen";
          "[fullscreen] <C-b>" = "scroll left";
          "[fullscreen] <C-n>" = "scroll down";
          "[fullscreen] <C-p>" = "scroll up";
          "[fullscreen] <C-f>" = "scroll right";
          "[fullscreen] <C-g>" = "abort";
          "[fullscreen] <C-[>" = "abort";
          "[fullscreen] <A-\\<>" = "goto top";
          "[fullscreen] <A-\\>>" = "goto bottom";
          "[fullscreen] a" = "adjust_window best-fit";
          "[fullscreen] s" = "adjust_window width";
          "[fullscreen] F" = "display_link";
          "[fullscreen] <C-c>" = "copy_link";
          "[fullscreen] f" = "follow";
          "[fullscreen] m" = "mark_add";
          "[fullscreen] \\'" = "mark_evaluate";
          "[fullscreen] \\," = "navigate next";
          "[fullscreen] \\." = "navigate previous";
          "[fullscreen] <A-Right>" = "navigate next";
          "[fullscreen] <A-Left>" = "navigate previous";
          "[fullscreen] <C-P>" = "print";
          "[fullscreen] c" = "recolor";
          "[fullscreen] R" = "reload";
          "[fullscreen] v" = "rotate rotate_cw";
          "[fullscreen] V" = "rotate rotate_ccw";
          "[fullscreen] <Left>" = "scroll left";
          "[fullscreen] <Up>" = "scroll up";
          "[fullscreen] <Down>" = "scroll down";
          "[fullscreen] <Right>" = "scroll right";
          "[fullscreen] <A-a>" = "scroll half-left";
          "[fullscreen] <C-V>" = "scroll half-down";
          "[fullscreen] <A-V>" = "scroll half-up";
          "[fullscreen] <A-e>" = "scroll half-right";
          "[fullscreen] <C-a>" = "scroll full-left";
          "[fullscreen] <C-v>" = "scroll full-down";
          "[fullscreen] <Return>" = "scroll full-down";
          "[fullscreen] <A-v>" = "scroll full-up";
          "[fullscreen] <C-e>" = "scroll full-right";
          "[fullscreen] <Space>" = "scroll full-down";
          "[fullscreen] <A-Space>" = "scroll full-up";
          "[fullscreen] <C-h>" = "scroll full-up";
          "[fullscreen] <BackSpace>" = "scroll full-up";
          "[fullscreen] <S-Space>" = "scroll full-up";
          "[fullscreen] l" = "jumplist backward";
          "[fullscreen] r" = "jumplist forward";
          "[fullscreen] <A-r>" = "bisect forward";
          "[fullscreen] <A-l>" = "bisect backward";
          "[fullscreen] <C-s>" = "search forward";
          "[fullscreen] <C-r>" = "search backward";
          "[fullscreen] p" = "snap_to_page";
          "[fullscreen] i" = "toggle_index";
          "[fullscreen] <C-i>" = "toggle_index";
          "[fullscreen] <Tab>" = "toggle_index";
          "[fullscreen] <A-s>" = "toggle_statusbar";
          "[fullscreen] <A-i>" = "focus_inputbar";
          "[fullscreen] d" = "toggle_page_mode";
          "[fullscreen] +" = "zoom in";
          "[fullscreen] -" = "zoom out";
          "[fullscreen] =" = "zoom in";
          "[index] <A-s>" = "toggle_statusbar";
          "[index] q" = "toggle_index";
          "[index] i" = "toggle_index";
          "[index] <C-p>" = "navigate_index up";
          "[index] <C-h>" = "navigate_index up";
          "[index] <BackSpace>" = "navigate_index up";
          "[index] <C-n>" = "navigate_index down";
          "[index] <A-v>" = "navigate_index up";
          "[index] <C-v>" = "navigate_index down";
          "[index] \\<" = "navigate_index top";
          "[index] \\>" = "navigate_index bottom";
          "[index] <A-\\<>" = "navigate_index top";
          "[index] <A-\\>>" = "navigate_index bottom";
          "[index] <C-b>" = "navigate_index collapse";
          "[index] <C-f>" = "navigate_index expand";
          "[index] <C-i>" = "navigate_index expand-all";
          "[index] <A-i>" = "navigate_index collapse-all";
          "[index] <Up>" = "navigate_index up";
          "[index] <Down>" = "navigate_index down";
          "[index] <Left>" = "navigate_index collapse";
          "[index] <Right>" = "navigate_index expand";
          "[index] <C-m>" = "navigate_index select";
          "[index] <Space>" = "navigate_index select";
          "[index] <Return>" = "navigate_index select";
          "[index] <C-j>" = "navigate_index select";
          "[index] <Esc>" = "toggle_index";
          "[index] <C-[>" = "toggle_index";
          "[index] <C-g>" = "toggle_index";
          "[index] <C-c>" = "toggle_index";
          "[presentation] i" = "toggle_index";
          "[presentation] r" = "navigate next";
          "[presentation] <Down>" = "navigate next";
          "[presentation] <Right>" = "navigate next";
          "[presentation] <PageDown>" = "navigate next";
          "[presentation] <Space>" = "navigate next";
          "[presentation] l" = "navigate previous";
          "[presentation] <Left>" = "navigate previous";
          "[presentation] <Up>" = "navigate previous";
          "[presentation] <PageUp>" = "navigate previous";
          "[presentation] <S-Space>" = "navigate previous";
          "[presentation] <BackSpace>" = "navigate previous";
          "[presentation] <F5>" = "toggle_presentation";
          "[presentation] q" = "toggle_presentation";
          "[presentation] <C-h>" = "navigate previous";
          "[presentation] <M-v>" = "navigate previous";
          "[presentation] <C-v>" = "navigate next";
          "[presentation] <A-\\<>" = "goto top";
          "[presentation] <A-\\>>" = "goto bottom";

        };
        options = {
          selection-clipboard = "clipboard";
          window-title-basename = true;
          adjust-open = "width";
          scroll-page-aware = false;
          scroll-full-overlap = "0.1";
          statusbar-home-tilde = true;
          synctex = true;
          font = "sans-serif bold 16";
          guioptions = "";
          zoom-step = 9;
          scroll-step = 80;
          scroll-hstep = 80;
        };
      };
      programs.ssh = {
        enable = true;
        hashKnownHosts = true;
        matchBlocks = let
          dotSshPath =
            "${config.home-manager.users.yc.home.homeDirectory}/.ssh/";
        in {
          "github.com" = {
            # github.com:ne9z
            user = "git";
          };
          "gitlab.com" = {
            # gitlab.com:john8931
            user = "git";
            identityFile = dotSshPath + "tub_latex_repo_key";
          };
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
      programs.foot = {
        enable = true;
        server.enable = true;
        settings = {
          main = {
            term = "foot-direct";
            font = "monospace:size=12";
          };

          url = { launch = "wl-copy \${url}"; };
          mouse = { hide-when-typing = "yes"; };
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
      xdg = {
        mimeApps = {
          enable = true;
          defaultApplications = {
            "application/pdf" = "org.pwmt.zathura.desktop";
            "image/jpeg" = "org.nomacs.ImageLounge.desktop";
          };
        };
        configFile = {
          "xournalpp/settings.xml" = {
            source = ./not-nix-config-files/xournalpp-settings.xml;
          };
          "xournalpp/toolbar.ini" = {
            source = ./not-nix-config-files/xournalpp-toolbar.ini;
          };
          "gpxsee/gpxsee.conf" = {
            source = ./not-nix-config-files/gpxsee.conf;
          };
          "sway/yc-sticky-keymap" = {
            source = ./not-nix-config-files/sway-yc-sticky-keymap;
          };
          "latexmk/latexmkrc" = { text = ''$pdf_previewer = "zathura"''; };
          "emacs/init.el" = { source = ./not-nix-config-files/emacs-init.el; };
          "yc.sh" = { source = ./not-nix-config-files/bashrc-config.sh; };
          "nomacs/Image Lounge.conf" = {
            source = ./not-nix-config-files/nomacs-config.conf;
          };
          "w3m/bookmark.html" = {
            source = ./not-nix-config-files/w3m-bookmark.html;
          };
          "fuzzel/fuzzel.ini" = {
            text = ''
              [main]
              font=sans-serif:size=18:weight=bold'';
          };
          "w3m/config" = { source = ./not-nix-config-files/w3m-config; };
          "w3m/keymap" = { source = ./not-nix-config-files/w3m-keymap; };
        };

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
              on-click-right =
                "${pkgs.brightnessctl}/bin/brightnessctl set 100%";
              on-click = "${pkgs.brightnessctl}/bin/brightnessctl set 1%";
              on-scroll-down =
                "${pkgs.brightnessctl}/bin/brightnessctl set 1%-";
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
        font-size = 24;
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
           bindsym --no-warn Mod4+o exec ${pkgs.foot}/bin/foot ${pkgs.tmux}/bin/tmux attach-session
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
            size = 13.0;
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
          output = { DP-4 = { scale = "2"; }; };
          input = {
            "type:keyboard" =
              (if (config.zfs-root.per-user.yc.modules.keyboard.enable) then {
                xkb_file = "$HOME/.config/sway/yc-sticky-keymap";
              } else
                { });
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
            "${pkgs.foot}/bin/foot ${pkgs.tmux}/bin/tmux attach-session";
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
    };
  };
}
