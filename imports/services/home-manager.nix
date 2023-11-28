{ pkgs, ... }: {
  home-manager.users.yc.services = {
    swayidle = {
      enable = true;
      events = [
        {
          event = "before-sleep";
          command =
            "${pkgs.swaylock}/bin/swaylock  --show-failed-attempts --ignore-empty-password --daemonize";
        }
        {
          event = "lock";
          command = "lock";
        }
      ];
      timeouts = [
        {
          timeout = 900;
          command =
            "${pkgs.swaylock}/bin/swaylock --show-failed-attempts --ignore-empty-password --daemonize";
        }
        {
          timeout = 30;
          command =
            "if ${pkgs.procps}/bin/pgrep --exact swaylock; then ${pkgs.systemd}/bin/systemctl suspend; fi";
        }
        {
          timeout = 910;
          command = "${pkgs.systemd}/bin/systemctl suspend";
        }
      ];
    };
  };
}
