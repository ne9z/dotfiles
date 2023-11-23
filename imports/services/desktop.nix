{
  services = {
    xserver = {
      enable = true;
      displayManager.sddm = {
        enable = true;
        wayland.enable = true;
        settings = { General = { GreeterEnvironment = "QT_FONT_DPI=288"; }; };
      };
    };
    logind = {
      extraConfig = ''
        HandlePowerKey=suspend
      '';
      lidSwitch = "suspend";
      lidSwitchDocked = "ignore";
      lidSwitchExternalPower = "suspend";
    };
    pipewire = {
      enable = mkDefault true;
      alsa.enable = true;
      pulse.enable = true;
    };
  };
}