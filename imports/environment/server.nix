{ pkgs, ... }: {
  environment = {
    etc = {
      "ssh/ssh_host_ed25519_key" = {
        source = "/oldroot/etc/ssh/ssh_host_ed25519_key";
        mode = "0600";
      };
      "ssh/ssh_host_rsa_key" = {
        source = "/oldroot/etc/ssh/ssh_host_rsa_key";
        mode = "0600";
      };
    };
    systemPackages = builtins.attrValues {
      inherit (pkgs)
        smartmontools darkhttpd pyrosimple woeusb _7zz exfatprogs emacs-nox;
    };
    loginShellInit = ''
      dsrv () {
        darkhttpd . --addr ::1 --ipv6 --port 8088
      }
      Nb () {
        if test -z "$TMUX"; then
           echo 'not in tmux';
           return 1
        fi
        nixos-rebuild boot \
         --flake git+file:///home/yc/githost/systemConfiguration
      }
      Ns () {
        if test -z "$TMUX"; then
           echo 'not in tmux';
           return 1
        fi
        nixos-rebuild switch \
         --flake git+file:///home/yc/githost/systemConfiguration
      }
      tm () {
         tmux attach-session
      }
      e () {
        $EDITOR $@
      }
    '';
  };
}
