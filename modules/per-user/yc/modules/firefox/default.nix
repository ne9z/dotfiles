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
            name = "ublock"; # Has to be unique!
            url =
              "https://addons.mozilla.org/firefox/downloads/file/4121906/ublock_origin-1.50.0.xpi";
            hash = "sha256-fKYNmWIMq/peKNFBE6FCW62ifmIExmthS9fn2BPNbUs=";
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
          lockPref("apz.allow_double_tap_zooming", false);
          lockPref("apz.allow_zooming", false);
          lockPref("apz.gtk.touchpad_pinch.enabled", false);
          lockPref("beacon.enabled", false);
          lockPref("browser.aboutHomeSnippets.updateUrl", "");
          lockPref("browser.backspace_action", 0);
          lockPref("browser.casting.enabled", false);
          lockPref("browser.chrome.site_icons", false);
          lockPref("browser.contentblocking.category", "strict");
          lockPref("browser.display.use_document_fonts", 0);
          lockPref("browser.fixup.alternate.enabled", false);
          lockPref("browser.fixup.hide_user_pass", true);
          lockPref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr", false);
          lockPref("browser.newtabpage.activity-stream.feeds.topsites", false);
          lockPref("browser.newtabpage.activity-stream.showSponsoredTopSites", false);
          lockPref("browser.search.countryCode", "US");
          lockPref("browser.search.geoSpecificDefaults", false);
          lockPref("browser.search.geoip.url", "");
          lockPref("browser.search.region", "US");
          lockPref("browser.search.suggest.enabled", false);
          lockPref("browser.search.update", false);
          lockPref("browser.send_pings", false);
          lockPref("browser.send_pings.require_same_host", true);
          lockPref("browser.startup.homepage_override.buildID", "20100101");
          lockPref("browser.tabs.firefox-view", false);
          lockPref("browser.tabs.inTitlebar", 0);
          lockPref("browser.topsites.contile.enabled", false);
          lockPref("browser.uidensity", 1);
          lockPref("browser.urlbar.autoFill", false);
          lockPref("browser.urlbar.filter.javascript", true);
          lockPref("browser.urlbar.quicksuggest.enabled", false);
          lockPref("browser.urlbar.suggest.searches", false);
          lockPref("browser.urlbar.trimURLs", false);
          lockPref("camera.control.face_detection.enabled", false);
          lockPref("clipboard.autocopy", false);
          lockPref("device.sensors.enabled", false);
          lockPref("devtools.chrome.enabled", false);
          lockPref("devtools.debugger.force-local", true);
          lockPref("devtools.debugger.remote-enabled", false);
          lockPref("devtools.webide.autoinstallADBHelper", false);
          lockPref("devtools.webide.autoinstallFxdtAdapters", false);
          lockPref("devtools.webide.enabled", false);
          lockPref("dom.allow_cut_copy", false);
          lockPref("dom.archivereader.enabled", false);
          lockPref("dom.battery.enabled", false);
          lockPref("dom.enable_performance", false);
          lockPref("dom.enable_user_timing", false);
          lockPref("dom.event.clipboardevents.enabled", false);
          lockPref("dom.flyweb.enabled", false);
          lockPref("dom.gamepad.enabled", false);
          lockPref("dom.mozTCPSocket.enabled", false);
          lockPref("dom.netinfo.enabled", false);
          lockPref("dom.network.enabled", false);
          lockPref("dom.security.https_only_mode", true);
          lockPref("dom.serviceWorkers.enabled", false);
          lockPref("dom.telephony.enabled", false);
          lockPref("dom.vibrator.enabled", false);
          lockPref("dom.vr.enabled", false);
          lockPref("dom.webaudio.enabled", false);
          lockPref("dom.webnotifications.enabled", false);
          lockPref("extensions.webextensions.restrictedDomains", "");
          lockPref("general.buildID.override", "20100101");
          lockPref("general.smoothScroll", false);
          lockPref("geo.enabled", false);
          lockPref("geo.wifi.logging.enabled", false);
          lockPref("geo.wifi.uri", "https://location.services.mozilla.com/v1/geolocate?key=%MOZILLA_API_KEY%");
          lockPref("gfx.font_rendering.opentype_svg.enabled", false);
          lockPref("intl.accept_languages", "en");
          lockPref("intl.locale.matchOS", false);
          lockPref("javascript.options.asmjs", false);
          lockPref("javascript.options.wasm", false);
          lockPref("javascript.use_us_english_locale", true);
          lockPref("media.ffmpeg.low-latency.enabled", true);
          lockPref("media.ffmpeg.vaapi.enabled", true);
          lockPref("media.gmp-gmpopenh264.enabled", false);
          lockPref("media.gmp-manager.url", "");
          lockPref("media.navigator.mediadatadecoder_vpx_enabled", true);
          lockPref("media.peerconnection.ice.no_host", true);
          lockPref("media.video_stats.enabled", false);
          lockPref("media.webspeech.recognition.enable", false);
          lockPref("media.webspeech.synth.enabled", false);
          lockPref("network.IDN_show_punycode", true);
          lockPref("network.dns.blockDotOnion", true);
          lockPref("network.http.speculative-parallel-limit", 0);
          lockPref("network.jar.open-unsafe-types", false);
          lockPref("network.manage-offline-status", false);
          lockPref("network.proxy.socks", "localhost");
          lockPref("network.proxy.socks_port", 9050);
          lockPref("network.proxy.socks_remote_dns", true);
          lockPref("privacy.resistFingerprinting", true);
          lockPref("privacy.resistFingerprinting.block_mozAddonManager", true);
          lockPref("privacy.userContext.enabled", true);
          lockPref("security.mixed_content.block_active_content", true);
          lockPref("security.mixed_content.block_display_content", true);
          lockPref("security.xpconnect.plugin.unrestricted", false);
          lockPref("toolkit.zoomManager.zoomValues", "1,1.7,2,2.3");
          lockPref("webgl.disable-extensions", true);
          lockPref("webgl.disable-fail-if-major-performance-caveat", true);
          lockPref("webgl.disabled", true);
          lockPref("webgl.enable-debug-renderer-info", false);
          lockPref("webgl.min_capability_mode", true);
        '';
      })
    ];
  };
}
