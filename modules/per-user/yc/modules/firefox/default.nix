{ config, lib, pkgs, ... }:
let
  cfg = config.zfs-root.per-user.yc.modules.firefox;
  inherit (lib) mkOption types mkIf;
in {
  options.zfs-root.per-user.yc.modules.firefox = {
    enable = mkOption {
      type = types.bool;
      default = config.zfs-root.per-user.yc.enable;
    };
  };
  config = mkIf cfg.enable {
    environment.systemPackages = [
      (pkgs.wrapFirefox pkgs.firefox-esr-unwrapped {
        nixExtensions = [
          (pkgs.fetchFirefoxAddon {
            name = "nojs"; # Has to be unique!
            url =
              "https://addons.mozilla.org/firefox/downloads/file/4111078/noscript-11.4.22.xpi";
            hash = "sha256-RqhMhd9Ma+EZBTiK0rZq61oBDtYVSEESyMFxCtE7w24=";
          })
        ];
        extraPolicies = {
          "3rdparty" = {
            Extensions = {
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
          CaptivePortal = true;
          DisableBuiltinPDFViewer = true;
          DisableFirefoxAccounts = true;
          DisableFirefoxStudies = true;
          DisableFormHistory = true;
          DisablePocket = true;
          DisableTelemetry = true;
          DisplayMenuBar = "never";
          DNSOverHTTPS = { Enabled = false; };
          EncryptedMediaExtensions = { Enabled = false; };
          FirefoxHome = {
            SponsoredTopSites = false;
            Pocket = false;
            SponsoredPocket = false;
          };
          HardwareAcceleration = true;
          Homepage = { StartPage = "none"; };
          NetworkPrediction = false;
          NewTabPage = false;
          NoDefaultBookmarks = false;
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
          user_pref("apz.allow_double_tap_zooming", false);
          user_pref("apz.allow_zooming", false);
          user_pref("apz.gtk.touchpad_pinch.enabled", false);
          user_pref("beacon.enabled", false);
          user_pref("browser.aboutHomeSnippets.updateUrl", "");
          user_pref("browser.backspace_action", 0);
          user_pref("browser.casting.enabled", false);
          user_pref("browser.chrome.site_icons", false);
          user_pref("browser.contentblocking.category", "strict");
          user_pref("browser.display.use_document_fonts", 0);
          user_pref("browser.fixup.alternate.enabled", false);
          user_pref("browser.fixup.hide_user_pass", true);
          user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr", false);
          user_pref("browser.newtabpage.activity-stream.feeds.topsites", false);
          user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false);
          user_pref("browser.search.countryCode", "US");
          user_pref("browser.search.geoSpecificDefaults", false);
          user_pref("browser.search.geoip.url", "");
          user_pref("browser.search.region", "US");
          user_pref("browser.search.suggest.enabled", false);
          user_pref("browser.search.update", false);
          user_pref("browser.send_pings", false);
          user_pref("browser.send_pings.require_same_host", true);
          user_pref("browser.startup.homepage_override.buildID", "20100101");
          user_pref("browser.tabs.firefox-view", false);
          user_pref("browser.tabs.inTitlebar", 0);
          user_pref("browser.topsites.contile.enabled", false);
          user_pref("browser.uidensity", 1);
          user_pref("browser.urlbar.autoFill", false);
          user_pref("browser.urlbar.filter.javascript", true);
          user_pref("browser.urlbar.quicksuggest.enabled", false);
          user_pref("browser.urlbar.suggest.searches", false);
          user_pref("browser.urlbar.trimURLs", false);
          user_pref("camera.control.face_detection.enabled", false);
          user_pref("clipboard.autocopy", false);
          user_pref("device.sensors.enabled", false);
          user_pref("devtools.chrome.enabled", false);
          user_pref("devtools.debugger.force-local", true);
          user_pref("devtools.debugger.remote-enabled", false);
          user_pref("devtools.webide.autoinstallADBHelper", false);
          user_pref("devtools.webide.autoinstallFxdtAdapters", false);
          user_pref("devtools.webide.enabled", false);
          user_pref("dom.allow_cut_copy", false);
          user_pref("dom.archivereader.enabled", false);
          user_pref("dom.battery.enabled", false);
          user_pref("dom.enable_performance", false);
          user_pref("dom.enable_user_timing", false);
          user_pref("dom.event.clipboardevents.enabled", false);
          user_pref("dom.flyweb.enabled", false);
          user_pref("dom.gamepad.enabled", false);
          user_pref("dom.mozTCPSocket.enabled", false);
          user_pref("dom.netinfo.enabled", false);
          user_pref("dom.network.enabled", false);
          user_pref("dom.security.https_only_mode", true);
          user_pref("dom.serviceWorkers.enabled", false);
          user_pref("dom.telephony.enabled", false);
          user_pref("dom.vibrator.enabled", false);
          user_pref("dom.vr.enabled", false);
          user_pref("dom.webaudio.enabled", false);
          user_pref("dom.webnotifications.enabled", false);
          user_pref("extensions.webextensions.restrictedDomains", "");
          user_pref("general.buildID.override", "20100101");
          user_pref("general.smoothScroll", false);
          user_pref("geo.enabled", false);
          user_pref("geo.wifi.logging.enabled", false);
          user_pref("geo.wifi.uri", "https://location.services.mozilla.com/v1/geolocate?key=%MOZILLA_API_KEY%");
          user_pref("gfx.font_rendering.opentype_svg.enabled", false);
          user_pref("intl.accept_languages", "en-US = en");
          user_pref("intl.locale.matchOS", false);
          user_pref("javascript.options.asmjs", false);
          user_pref("javascript.options.wasm", false);
          user_pref("javascript.use_us_english_locale", true);
          user_pref("media.ffmpeg.low-latency.enabled", true);
          user_pref("media.ffmpeg.vaapi.enabled", true);
          user_pref("media.gmp-gmpopenh264.enabled", false);
          user_pref("media.gmp-manager.url", "");
          user_pref("media.navigator.mediadatadecoder_vpx_enabled", true);
          user_pref("media.peerconnection.ice.no_host", true);
          user_pref("media.video_stats.enabled", false);
          user_pref("media.webspeech.recognition.enable", false);
          user_pref("media.webspeech.synth.enabled", false);
          user_pref("network.IDN_show_punycode", true);
          user_pref("network.dns.blockDotOnion", true);
          user_pref("network.http.speculative-parallel-limit", 0);
          user_pref("network.jar.open-unsafe-types", false);
          user_pref("network.manage-offline-status", false);
          user_pref("network.proxy.socks", "localhost");
          user_pref("network.proxy.socks_port", 9050);
          user_pref("network.proxy.socks_remote_dns", true);
          user_pref("privacy.resistFingerprinting", true);
          user_pref("privacy.resistFingerprinting.block_mozAddonManager", true);
          user_pref("privacy.userContext.enabled", true);
          user_pref("security.mixed_content.block_active_content", true);
          user_pref("security.mixed_content.block_display_content", true);
          user_pref("security.xpconnect.plugin.unrestricted", false);
          user_pref("toolkit.zoomManager.zoomValues", "1,1.7,2,2.3");
          user_pref("webgl.disable-extensions", true);
          user_pref("webgl.disable-fail-if-major-performance-caveat", true);
          user_pref("webgl.disabled", true);
          user_pref("webgl.enable-debug-renderer-info", false);
          user_pref("webgl.min_capability_mode", true);
        '';
      })
    ];
  };
}
