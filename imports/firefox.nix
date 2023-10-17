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
  extraPrefs = builtins.readFile ./firefox-user.js;
}
