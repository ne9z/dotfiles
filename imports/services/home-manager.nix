{ pkgs, ... }: {
  home-manager.users.yc.services = {
    swayidle = {
      enable = true;
      events = [
        {
          event = "before-sleep";
          command = "${pkgs.swaylock}/bin/swaylock  --show-failed-attempts --ignore-empty-password";
        }
        {
          event = "lock";
          command = "lock";
        }
      ];
      timeouts = [
        {
          timeout = 900;
          command = "${pkgs.swaylock}/bin/swaylock --show-failed-attempts --ignore-empty-password";
        }
        {
          timeout = 910;
          command = "${pkgs.systemd}/bin/systemctl suspend";
        }
      ];
    };
  };
}
