{ pkgs, ... }:
pkgs.wrapFirefox pkgs.firefox-esr-unwrapped {
  nixExtensions = [
    (pkgs.fetchFirefoxAddon {
      name = "ublock"; # Has to be unique!
      url =
        "https://codeberg.org/m0p/ublock-origin-mirror/raw/branch/main/ublock_origin-1.52.2.xpi";
      hash = "sha256-6O4/nVl6bULbnXP+h8HVId40B1X9i/3WnkFiPt/gltY=";
      fixedExtid = "uBlock0@raymondhill.net";
    })
  ];

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
      BehaviorPrivateBrowsing = "reject-tracker-and-partition-foreign";
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
    DontCheckDefaultBrowser = true;
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
    pref("javascript.options.ion", false);
    pref("javascript.options.baselinejit", false);
    pref("font.name-list.emoji", "Noto Color Emoji");
    pref("font.name.monospace.zh-CN", "Noto Sans Mono CJK SC");
    pref("font.name.sans-serif.zh-CN", "Noto Serif CJK SC");
    pref("font.name.serif.zh-CN", "Noto Serif CJK SC");
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
    pref("toolkit.zoomManager.zoomValues", "0.8,1,1.5,2");
    pref("webgl.disable-extensions", true);
    pref("webgl.disable-fail-if-major-performance-caveat", true);
    pref("webgl.disabled", true);
    pref("webgl.enable-debug-renderer-info", false);
    pref("webgl.min_capability_mode", true);
  '';
}
