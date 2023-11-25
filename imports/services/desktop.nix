{ pkgs, ... }:
{
  services = {
    xserver = {
      enable = true;
      displayManager.sddm = {
        enable = true;
        wayland.enable = true;
        settings = { General = { GreeterEnvironment = "QT_FONT_DPI=288,XCURSOR_SIZE=48,XCURSOR_THEME=Adwaita,XCURSOR_PATH=${pkgs.adwaita-icon-theme}/share/icons"; }; };
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
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
    };
  };
  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      package = pkgs.qemu_kvm;
      runAsRoot = false;
    };
  };
}
