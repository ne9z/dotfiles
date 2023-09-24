{ config, lib, pkgs, ... }:
let
  inherit (lib) mkDefault mkOption types mkIf;
  # buildEmacs is a function that takes a set of emacs packages as input
  buildEmacs = (pkgs.emacsPackagesFor pkgs.emacs29-pgtk).emacsWithPackages;
  emacsPkg = buildEmacs (epkgs:
    builtins.attrValues {
      inherit (epkgs.melpaPackages) nix-mode magit;
      inherit (epkgs.elpaPackages) auctex pyim pyim-basedict;
      inherit (epkgs.treesit-grammars) with-all-grammars;
    });
  firefoxPkg = (pkgs.wrapFirefox pkgs.firefox-esr-unwrapped {
    extraPolicies = {
      "3rdparty" = {
        Extensions = {
          # name must be the same as above
          "uBlock0@raymondhill.net" = {
            adminSettings = {
              userSettings = {
                advancedUserEnabled = true;
                popupPanelSections = 31;
              };
              dynamicFilteringString = ''
                * * inline-script block
                * * 1p-script block
                * * 3p block
                * * 3p-script block
                * * 3p-frame block'';
              hostnameSwitchesString = ''
                no-cosmetic-filtering: * true
                no-remote-fonts: * true
                no-csp-reports: * true
                no-scripting: * true
              '';
            };
          };
        };
      };
      # captive portal enabled for connecting to free wifi
      CaptivePortal = false;
      Cookies = {
        Behavior = "reject-tracker-and-partition-foreign";
        BehaviorPrivateBrowsing =
          "reject-tracker-and-partition-foreign";
        ExpireAtSessionEnd = true;
      };
      DisableBuiltinPDFViewer = true;
      DisableFirefoxAccounts = true;
      DisableFirefoxStudies = true;
      DisableFormHistory = true;
      DisablePocket = true;
      DisableTelemetry = true;
      DisplayMenuBar = "never";
      DNSOverHTTPS = { Enabled = false; };
      EncryptedMediaExtensions = { Enabled = false; };
      ExtensionUpdate = false;
      FirefoxHome = {
        SponsoredTopSites = false;
        Pocket = false;
        SponsoredPocket = false;
      };
      HardwareAcceleration = true;
      Homepage = { StartPage = "none"; };
      NetworkPrediction = false;
      NewTabPage = false;
      NoDefaultBookmarks = true;
      OfferToSaveLogins = false;
      OfferToSaveLoginsDefault = false;
      OverrideFirstRunPage = "";
      OverridePostUpdatePage = "";
      PasswordManagerEnabled = false;
      PDFjs = { Enabled = false; };
      Permissions = {
        Location = { BlockNewRequests = true; };
        Notifications = { BlockNewRequests = true; };
      };
      PictureInPicture = { Enabled = false; };
      PopupBlocking = { Default = false; };
      PromptForDownloadLocation = true;
      SanitizeOnShutdown = true;
      SearchSuggestEnabled = false;
      ShowHomeButton = true;
      UserMessaging = {
        WhatsNew = false;
        SkipOnboarding = true;
      };
    };
    extraPrefs = ''
      lockPref("browser.urlbar.suggest.quicksuggest.sponsored", false);
      pref("browser.urlbar.suggest.quicksuggest.nonsponsored", false);
      pref("apz.allow_double_tap_zooming", false);
      pref("apz.allow_zooming", false);
      pref("apz.gtk.touchpad_pinch.enabled", false);
      pref("beacon.enabled", false);
      pref("browser.aboutConfig.showWarning", false);
      pref("browser.aboutHomeSnippets.updateUrl", "");
      pref("browser.backspace_action", 0);
      pref("browser.casting.enabled", false);
      pref("browser.chrome.site_icons", false);
      pref("browser.contentblocking.category", "strict");
      pref("browser.display.use_document_fonts", 0);
      pref("browser.fixup.alternate.enabled", false);
      pref("browser.fixup.hide_user_pass", true);
      pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr", false);
      pref("browser.newtabpage.activity-stream.feeds.topsites", false);
      pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false);
      pref("browser.search.countryCode", "US");
      pref("browser.search.geoSpecificDefaults", false);
      pref("browser.search.geoip.url", "");
      pref("browser.search.region", "US");
      pref("browser.search.suggest.enabled", false);
      pref("browser.search.update", false);
      pref("browser.send_pings", false);
      pref("browser.send_pings.require_same_host", true);
      pref("browser.startup.homepage_override.buildID", "20100101");
      pref("browser.tabs.firefox-view", false);
      pref("browser.tabs.inTitlebar", 0);
      pref("browser.topsites.contile.enabled", false);
      pref("browser.uidensity", 1);
      pref("browser.urlbar.autoFill", false);
      pref("browser.urlbar.filter.javascript", true);
      pref("browser.urlbar.quicksuggest.enabled", false);
      pref("browser.urlbar.suggest.searches", false);
      pref("browser.urlbar.trimURLs", false);
      pref("camera.control.face_detection.enabled", false);
      pref("clipboard.autocopy", false);
      pref("device.sensors.enabled", false);
      pref("devtools.chrome.enabled", false);
      pref("devtools.debugger.force-local", true);
      pref("devtools.debugger.remote-enabled", false);
      pref("devtools.webide.autoinstallADBHelper", false);
      pref("devtools.webide.autoinstallFxdtAdapters", false);
      pref("devtools.webide.enabled", false);
      pref("dom.allow_cut_copy", false);
      pref("dom.archivereader.enabled", false);
      pref("dom.battery.enabled", false);
      pref("dom.enable_performance", false);
      pref("dom.enable_user_timing", false);
      pref("dom.event.clipboardevents.enabled", false);
      pref("dom.flyweb.enabled", false);
      pref("dom.gamepad.enabled", false);
      pref("dom.mozTCPSocket.enabled", false);
      pref("dom.netinfo.enabled", false);
      pref("dom.network.enabled", false);
      pref("dom.security.https_only_mode", true);
      pref("dom.serviceWorkers.enabled", false);
      pref("dom.telephony.enabled", false);
      pref("dom.vibrator.enabled", false);
      pref("dom.vr.enabled", false);
      pref("dom.webaudio.enabled", false);
      pref("dom.webnotifications.enabled", false);
      pref("extensions.webextensions.restrictedDomains", "");
      pref("general.buildID.override", "20100101");
      pref("general.smoothScroll", false);
      pref("geo.enabled", false);
      pref("geo.wifi.logging.enabled", false);
      pref("geo.wifi.uri", "https://location.services.mozilla.com/v1/geolocate?key=%MOZILLA_API_KEY%");
      pref("gfx.font_rendering.opentype_svg.enabled", false);
      pref("intl.accept_languages", "en");
      pref("intl.locale.matchOS", false);
      pref("javascript.enabled", false);
      pref("javascript.options.asmjs", false);
      pref("javascript.options.wasm", false);
      pref("javascript.use_us_english_locale", true);
      pref("media.ffmpeg.low-latency.enabled", true);
      pref("media.ffmpeg.vaapi.enabled", true);
      pref("media.gmp-gmpopenh264.enabled", false);
      pref("media.gmp-manager.url", "");
      pref("media.navigator.mediadatadecoder_vpx_enabled", true);
      pref("media.peerconnection.ice.no_host", true);
      pref("media.video_stats.enabled", false);
      pref("media.webspeech.recognition.enable", false);
      pref("media.webspeech.synth.enabled", false);
      pref("network.IDN_show_punycode", true);
      pref("network.dns.blockDotOnion", true);
      pref("network.http.speculative-parallel-limit", 0);
      pref("network.jar.open-unsafe-types", false);
      pref("network.manage-offline-status", false);
      pref("network.proxy.socks", "localhost");
      pref("network.proxy.socks_port", 9050);
      pref("network.proxy.socks_remote_dns", true);
      pref("privacy.resistFingerprinting", true);
      pref("privacy.resistFingerprinting.block_mozAddonManager", true);
      pref("privacy.userContext.enabled", true);
      pref("security.mixed_content.block_active_content", true);
      pref("security.mixed_content.block_display_content", true);
      pref("security.xpconnect.plugin.unrestricted", false);
      pref("toolkit.zoomManager.zoomValues", "0.5,0.8,1");
      pref("webgl.disable-extensions", true);
      pref("webgl.disable-fail-if-major-performance-caveat", true);
      pref("webgl.disabled", true);
      pref("webgl.enable-debug-renderer-info", false);
      pref("webgl.min_capability_mode", true);
    '';
  });
  mytex = (pkgs.texlive.combine {
    inherit (pkgs.texlive)
      collection-basic collection-mathscience
      # languages
      collection-langenglish collection-langgerman
      # pdf manipulation tool
      pdfjam # depends on pdfpages, geometry
      # pdfpages and dependencies
      pdfpages eso-pic atbegshi pdflscape
      # century schoolbook
      schola-otf iftex xkeyval unicode-math fontspec realscripts lualatex-math
      tex-gyre tex-gyre-math libertinus-otf libertinus-fonts stix2-otf
      # beamer
      beamer etoolbox hyperref pgf
      # cjk
      #luatexja chinese-jfm
      # preview-latex
      dvips luatex85
      # bold computer modern
      newcomputermodern fontsetup
      # pictures and tikz
      collection-pictures;
  });
in {
  users.mutableUsers = false;
  users.users = {
    yc = {
      # "!" means login disabled
      initialHashedPassword =
        "$6$UxT9KYGGV6ik$BhH3Q.2F8x1llZQLUS1Gm4AxU7bmgZUP7pNX6Qt3qrdXUy7ZYByl5RVyKKMp/DuHZgk.RiiEXK8YVH.b2nuOO/";
      description = "Yuchen Guo";
      # a default group must be set
      extraGroups = [
        # use doas
        "wheel"
        # manage VMs
        "libvirtd"
        # net
        "networkmanager"
      ];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICkDT9xZLh+lHc6Z60oLZlLjzOcP39B3D7ptV6xSzAhu openpgp:0x464B6BB1"
      ];
      packages = builtins.attrValues {
        inherit (pkgs)
          ffmpeg mg nixfmt qrencode zathura jmtpfs gpxsee
          # pdf processor in Go
          pdfcpu
          # image editor
          nomacs
          # CoMa programs
          python3 remmina
          # pdf manipulation suite in C++
          # https://qpdf.readthedocs.io/en/stable/
          qpdf
          # preview-latex
          ghostscript;
      };
      isNormalUser = true;
      uid = 1000;
    };
  };
  hardware = {
    opengl = {
      extraPackages =
        builtins.attrValues { inherit (pkgs) vaapiIntel intel-media-driver; };
      enable = true;
    };
    bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
    pulseaudio.enable = false;
  };
  services = {
    blueman.enable = true;
    logind = {
      extraConfig = ''
        HandlePowerKey=suspend
      '';
      lidSwitch = "suspend";
      lidSwitchDocked = "ignore";
      lidSwitchExternalPower = "suspend";
    };
    pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
    };
  };
  sound.enable = true;
  programs.sway = {
    extraSessionCommands = ''
      export MOZ_ENABLE_WAYLAND=1
      export XCURSOR_THEME=Adwaita
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';
    enable = true;
    extraPackages = builtins.attrValues {
      inherit (pkgs)
        swaylock swayidle foot gammastep wl-gammactl brightnessctl fuzzel grim
        libva-utils w3m gsettings-desktop-schemas pavucontrol waybar
        wl-clipboard wf-recorder;
    };
    # must be enabled, or else many programs will crash
    wrapperFeatures.gtk = true;
  };
  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };
  fonts.fontconfig = {
    defaultFonts = {
      monospace = [ "Libertinus Mono" "Noto Sans Mono CJK SC" ];
      sansSerif = [ "TeX Gyre Schola" "Libertinus Serif" "Noto Sans CJK SC" ];
      serif = [ "TeX Gyre Schola" "Libertinus Serif" "Noto Sans CJK SC" ];
    };
  };
  fonts.packages = builtins.attrValues {
    inherit (pkgs)
      dejavu_fonts stix-two noto-fonts libertinus gyre-fonts
      # noto cjk
      noto-fonts-cjk-serif noto-fonts-cjk-sans;
  };
  environment.sessionVariables = {
    VAAPI_DISABLE_INTERLACE = "1";
    QT_AUTO_SCREEN_SCALE_FACTOR = "1";
    QT_WAYLAND_FORCE_DPI = mkDefault "physical";
    GDK_DPI_SCALE = mkDefault "2";
  };
  services.dictd = {
    enable = true;
    DBs = builtins.attrValues { inherit (pkgs.dictdDBs) wordnet; };
  };
  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      package = pkgs.qemu_kvm;
      runAsRoot = false;
    };
  };
  environment = {
    systemPackages = builtins.attrValues {
      inherit (pkgs) virt-manager;
      inherit (pkgs) poppler perl;
      inherit (pkgs)
      # spell checkers
        hunspell
        # used with dired mode to open files
        xdg-utils;
      inherit (pkgs.hunspellDicts) en_US de_DE;
      inherit emacsPkg mytex;
    };
    interactiveShellInit = ''
      export EDITOR="${emacsPkg}/bin/emacsclient --alternate-editor= --create-frame"
      e () { $EDITOR "$@"; }
    '';
  };
  # have a clean home
  zfs-root.fileSystems.datasets = { "rpool/nixos/home" = "/oldroot/home"; };
  fileSystems."/home/yc" = {
    device = "tmpfs";
    fsType = "tmpfs";
    options = [ "rw" "size=1G" "uid=yc" "gid=users" "mode=1700" ];
  };
  home-manager.users.yc = {
    programs.firefox = {
      enable = true;
      package = firefoxPkg;
      profiles.default = {
        id = 0;
        name = "Default";
        isDefault = true;
        extensions = builtins.attrValues {
          inherit (pkgs.nur.repos.rycee.firefox-addons) ublock-origin;
        };
      };
    };
    home = {
      username = "yc";
      homeDirectory = mkDefault "/home/yc";
      stateVersion = config.system.stateVersion;
    };
    programs.home-manager.enable = true;
    home = {
      packages = builtins.attrValues {
        inherit (pkgs)
          mg shellcheck _7zz xournalpp
          # book scanning
          poppler_utils libtiff scantailor-advanced;
      };
    };

    gtk = {
      enable = true;
      font = { name = "sans-serif"; };
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
        document-font-name = "sans-serif";
        monospace-font-name = "monospace";
        gtk-key-theme = "Emacs";
        cursor-size = 48;
        scaling-factor = 2;
      };
    };
    programs.bash = {
      enable = true;
      initExtra = "if [ -f ~/.config/yc.sh ]; then source ~/.config/yc.sh; fi";
    };
    programs = {
      git = {
        enable = true;
        userEmail = "yguo@posteo.net";
        userName = "Yuchen Guo";
      };
      # use gnus instead
      mbsync.enable = false;
      msmtp.enable = false;
      notmuch.enable = false;
    };
    programs.mpv = {
      enable = true;
      config = {
        hwdec = "vaapi";
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
        dotSshPath = "${config.home-manager.users.yc.home.homeDirectory}/.ssh/";
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
          font = mkDefault "monospace:size=12";
          dpi-aware = "yes";
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
        "sway/yc-sticky-keymap" = {
          source = ./not-nix-config-files/sway-yc-sticky-keymap;
        };
        "latexmk/latexmkrc" = { text = ''$pdf_previewer = "zathura"''; };
        "emacs/init.el" = { source = ./not-nix-config-files/emacs-init.el; };
        "yc.sh" = { source = ./not-nix-config-files/bashrc-config.sh; };
        "nomacs/Image Lounge.conf" = {
          source = ./not-nix-config-files/nomacs-config.conf;
        };
        "scantailor-advanced/scantailor-advanced.ini" = {
          source = ./not-nix-config-files/scantailor-advanced.ini;
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
         bindsym --no-warn Mod4+o exec ${emacsPkg}/bin/emacsclient --alternate-editor= --create-frame
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
          "type:keyboard" = {
            xkb_file = "$HOME/.config/sway/yc-sticky-keymap";
          };
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
            background = "${pkgs.sway}/share/backgrounds/sway/Sway_Wallpaper_Blue_1920x1080.png fill";
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
  };
  console.useXkbConfig = true;
  services.xserver = {
    layout = "yc";
    extraLayouts."yc" = {
      description = "zfs-root layout";
      languages = [ "eng" ];
      symbolsFile = ./symbols.txt;
    };
  };
  environment.variables = { XKB_DEFAULT_LAYOUT = "yc"; };
}
