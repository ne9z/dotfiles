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
        {
          event = "after-resume";
          command = "${pkgs.coreutils-full}/bin/sleep 30; if ${pkgs.procps}/bin/pgrep --exact swaylock; then ${pkgs.systemd}/bin/systemctl suspend; fi";
        }
      ];
      timeouts = [
        {
          timeout = 900;
          command =
            "${pkgs.swaylock}/bin/swaylock --show-failed-attempts --ignore-empty-password --daemonize";
        }
        {
          timeout = 910;
          command = "${pkgs.systemd}/bin/systemctl suspend";
        }
      ];
    };
  };
}
