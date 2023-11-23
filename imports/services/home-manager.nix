{
  home-manager.users.yc.services = {
    emacs = {
      enable = true;
      package = emacsPkg;
      client.enable = true;
      client.arguments = [ "--create-frame" ];
      defaultEditor = true;
      startWithUserSession = "graphical";
    };
    swayidle = {
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
  };
}
