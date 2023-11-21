{ config, pkgs, lib, inputs, modulesPath, ... }:
let
  inherit (lib) mkMerge mapAttrsToList mkDefault;
  inherit (inputs) self nixpkgs;
in {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/hardened.nix")
  ];
  boot.initrd.systemd.enable = mkDefault true;
  # workaround for hardened profile
  services.logrotate.checkConfig = false;

  environment.etc."wpa_supplicant.conf".text = "#";

  security.lockKernelModules = false;

  boot.initrd.systemd.emergencyAccess =
    "$6$UxT9KYGGV6ik$BhH3Q.2F8x1llZQLUS1Gm4AxU7bmgZUP7pNX6Qt3qrdXUy7ZYByl5RVyKKMp/DuHZgk.RiiEXK8YVH.b2nuOO/";

  networking = {
    useDHCP = mkDefault true;
    useNetworkd = mkDefault true;
    hosts = { "200:8bcd:55f4:becc:4d85:2fa6:2ed2:5eba" = [ "tl.yc" ]; };
    nameservers = [ "::1" ];
  };
  services.stubby = {
    enable = true;
    settings = pkgs.stubby.passthru.settingsExample;
  };
  services.resolved = { enable = false; };

  zfs-root.boot.devNodes = "/dev/disk/by-id/";
  zfs-root.boot.immutable.enable = mkDefault true;

  users.mutableUsers = false;

  programs.tmux = {
    enable = true;
    keyMode = "emacs";
    newSession = true;
    secureSocket = true;
    terminal = "tmux-direct";
    historyLimit = 4096;
    baseIndex = 1;
    extraConfig = ''
      unbind C-b
      unbind f7

      set -u prefix
      set -g prefix f7
      bind -N "Send the prefix key through to the application" \
        f7 send-prefix

      bind-key -T prefix t new-session
      # toggle status bar with f7+f8
      set -g status off
      bind-key -T prefix f8 set-option -g status

      # disable cpu intensive auto-rename
      setw -g automatic-rename off

      # transparent status bar
      set-option -g status-style bg=default
    '';
  };

  services.tlp = {
    enable = true;
    settings = {
      # defaults for lenovo ideapads
      # Stop charging battery BAT0 and BAT1 at 60%:
      # https://linrunner.de/tlp/settings/bc-vendors.html
      START_CHARGE_THRESH_BAT0 = mkDefault 0;
      STOP_CHARGE_THRESH_BAT0 = mkDefault 1;
    };
  };
  boot.loader.grub.memtest86.enable = true;

  services.openssh = {
    enable = true;
    settings = { PasswordAuthentication = false; };
  };

  environment.memoryAllocator.provider = "libc";

  services = {
    tor = {
      enable = mkDefault false;
      client = {
        enable = true;
        dns.enable = true;
      };
      settings = {
        Sandbox = true;
        SafeSocks = 1;
        NoExec = 1;
      };
      torsocks = {
        enable = true;
        server = "127.0.0.1:9050";
      };
    };
    i2pd = {
      enable = mkDefault false;
      enableIPv4 = true;
      enableIPv6 = true;
      bandwidth = 4096;
      port = 29392;
      proto = {
        http = {
          port = 7071;
          enable = true;
        };
        socksProxy.port = 4447;
        socksProxy.enable = true;
      };
      floodfill = true;
      inTunnels = { };
      outTunnels = {
        # connect to mail services by postman
        # available at http://hq.postman.i2p
        smtp-postman = {
          enable = true;
          address = "::1";
          destinationPort = 7659;
          destination = "smtp.postman.i2p";
          port = 7659;
        };
        pop-postman = {
          enable = true;
          address = "::1";
          destinationPort = 7660;
          destination = "pop.postman.i2p";
          port = 7660;
        };
      };
    };

    yggdrasil = {
      enable = true;
      openMulticastPort = false;
      settings.Peers = [
        #curl -o test.html https://publicpeers.neilalexander.dev/
        #grep -e 'tls://' -e 'tcp://' test.html  | grep -v offline | sed 's|<td id="address">|"|' | sed 's|</td><td.*|"|g' | sort | wl-copy -n
        "tcp://0.ygg.l1qu1d.net:11100?key=0000000998b5ff8c0f1115ce9212f772d0427151f50fe858e6de1d22600f1680"
        "tcp://178.132.161.124:65533"
        "tcp://178.20.41.3:65533"
        "tcp://185.103.109.63:65533"
        "tcp://185.165.169.234:8880"
        "tcp://188.225.9.167:18226"
        "tcp://193.107.20.230:7743"
        "tcp://193.93.119.42:14244"
        "tcp://195.123.245.146:7743"
        "tcp://[2001:470:1f0a:3d7::2]:65533"
        "tcp://[2001:470:1f13:e56::64]:39565"
        "tcp://[2a00:b700:1::38]:65533"
        "tcp://[2a00:b700:2::5:2d4]:65533"
        "tcp://[2a00:b700:3::3:3b4]:65533"
        "tcp://[2a00:b700:4::3:6f]:65533"
        "tcp://[2a00:b700:5::5:34]:65533"
        "tcp://[2a00:b700::a:279]:12402"
        "tcp://[2a00:b700::c:2e1]:65533"
        "tcp://[2a05:9403::8b]:7743"
        "tcp://[2a09:5302:ffff::992]:12403"
        "tcp://[2a09:5302:ffff::ac9]:65533"
        "tcp://[2a09:5302:ffff::aca]:65533"
        "tcp://[2a0d:8480:1:2e::]:65533"
        "tcp://[2a0d:8480:2:6bd::]:65533"
        "tcp://[2a12:5940:1464::2]:65533"
        "tcp://[2a12:5940:b1a0::2]:65533"
        "tcp://45.147.200.202:12402"
        "tcp://45.95.202.21:12403"
        "tcp://45.95.202.91:65533"
        "tcp://51.15.204.214:12345"
        "tcp://62.210.85.80:39565"
        "tcp://77.37.218.131:12402"
        "tcp://77.91.84.76:65533"
        "tcp://78.27.153.163:33165"
        "tcp://79.137.194.94:65533"
        "tcp://80.78.27.103:30111"
        "tcp://87.251.77.39:65533"
        "tcp://88.210.3.30:65533"
        "tcp://94.130.203.208:5999"
        "tcp://bode.theender.net:42069"
        "tcp://ekb.itrus.su:7991"
        "tcp://itcom.multed.com:7991"
        "tcp://kusoneko.moe:9002"
        "tcp://mima.localghost.org:1996"
        "tcp://pp1.ygg.sy.sa:8441"
        "tcp://s-ams-0.sergeysedoy97.ru:65533"
        "tcp://s-ams-1.sergeysedoy97.ru:65533"
        "tcp://s-fra-0.sergeysedoy97.ru:65533"
        "tcp://sin.yuetau.net:6642"
        "tcp://s-kzn-0.sergeysedoy97.ru:65533"
        "tcp://s-led-0.sergeysedoy97.ru:65533"
        "tcp://s-mow-0.sergeysedoy97.ru:65533"
        "tcp://s-mow-1.sergeysedoy97.ru:65533"
        "tcp://s-mow-2.sergeysedoy97.ru:65533"
        "tcp://s-mow-3.sergeysedoy97.ru:65533"
        "tcp://s-mow-4.sergeysedoy97.ru:65533"
        "tcp://s-ovb-1.sergeysedoy97.ru:65533"
        "tcp://srv.itrus.su:7991"
        "tcp://supergay.network:9002"
        "tcp://vpn.itrus.su:7991"
        "tcp://x-ams-0.sergeysedoy97.ru:65533"
        "tcp://x-ams-1.sergeysedoy97.ru:65533"
        "tcp://x-fra-0.sergeysedoy97.ru:65533"
        "tcp://x-kiv-0.sergeysedoy97.ru:65533"
        "tcp://x-kzn-0.sergeysedoy97.ru:65533"
        "tcp://x-mow-0.sergeysedoy97.ru:65533"
        "tcp://x-mow-1.sergeysedoy97.ru:65533"
        "tcp://x-mow-3.sergeysedoy97.ru:65533"
        "tcp://x-mow-4.sergeysedoy97.ru:65533"
        "tcp://x-ovb-0.sergeysedoy97.ru:65533"
        "tcp://x-ovb-1.sergeysedoy97.ru:65533"
        "tcp://x-sto-0.sergeysedoy97.ru:65533"
        "tcp://ygg1.mk16.de:1337?key=0000000087ee9949eeab56bd430ee8f324cad55abf3993ed9b9be63ce693e18a"
        "tcp://ygg2.mk16.de:1337?key=000000d80a2d7b3126ea65c8c08fc751088c491a5cdd47eff11c86fa1e4644ae"
        "tcp://ygg3.mk16.de:1337?key=000003acdaf2a60e8de2f63c3e63b7e911d02380934f09ee5c83acb758f470c1"
        "tcp://ygg4.mk16.de:1337?key=0000147df8daa1cce2ad4b1d4b14c60a4c69a991b2dfde4e00ba7e95c36c530b"
        "tcp://ygg.ace.ctrl-c.liu.se:9998?key=5636b3af4738c3998284c4805d91209cab38921159c66a6f359f3f692af1c908"
        "tcp://yggdrasil-2.herronjo.com:1336?key=6cbcd23d94c9a300e442bd1054c7ced8d09dbb6349261651b24e76851efb7edf"
        "tcp://yggdrasil.community.garage.networks.deavmi.assigned.network:2000"
        "tcp://yggdrasil.su:62486"
        "tcp://ygg.iva.bz:50001"
        "tcp://ygg.mkg20001.io:80"
        "tcp://ygg-msk-1.averyan.ru:8363"
        "tcp://yggno.de:18226"
        "tcp://ygg.yt:80"
        "tcp://y.zbin.eu:7743"
        "tcp://zhoskiy.xyz:30111"
        "tls://0.ygg.l1qu1d.net:11101?key=0000000998b5ff8c0f1115ce9212f772d0427151f50fe858e6de1d22600f1680"
        "tls://139.28.220.141:3333"
        "tls://152.228.216.112:23108"
        "tls://178.132.161.124:65534"
        "tls://178.20.41.3:65534"
        "tls://185.103.109.63:65534"
        "tls://185.165.169.234:8443"
        "tls://185.175.90.87:43006"
        "tls://188.225.9.167:18227"
        "tls://192.99.145.61:58226"
        "tls://193.93.119.42:443"
        "tls://[2001:41d0:303:13b3:51:255:223:60]:54232"
        "tls://[2001:41d0:304:200::ace3]:23108"
        "tls://[2001:41d0:601:1100::cf2]:11129"
        "tls://[2001:41d0:801:2000::233f]:28395"
        "tls://[2001:470:1f0a:3d7::2]:65534"
        "tls://[2001:470:1f13:e56::64]:39575"
        "tls://23.137.249.65:444"
        "tls://23.137.251.45:5222"
        "tls://23.184.48.86:993"
        "tls://[2602:fc24:18:7a42::1]:993"
        "tls://[2607:5300:201:3100::50a1]:58226"
        "tls://[2a00:b700:1::38]:65534"
        "tls://[2a00:b700:2::5:2d4]:65534"
        "tls://[2a00:b700:3::3:3b4]:65534"
        "tls://[2a00:b700:4::3:6f]:65534"
        "tls://[2a00:b700:5::5:34]:65534"
        "tls://[2a00:b700::a:279]:443"
        "tls://[2a00:b700::c:2e1]:65534"
        "tls://[2a01:4f9:2a:60c::2]:18836"
        "tls://[2a01:4f9:c010:664d::1]:61995"
        "tls://[2a03:3b40:fe:ab::1]:993"
        "tls://[2a09:5302:ffff::992]:443"
        "tls://[2a09:5302:ffff::ac9]:65534"
        "tls://[2a09:5302:ffff::aca]:65534"
        "tls://[2a0a:9300:1:152::1]:3333"
        "tls://[2a0d:8480:1:2e::]:65534"
        "tls://[2a0d:8480:2:6bd::]:65534"
        "tls://[2a10:4740:40:0:2222:3f9c:b7cf:1]:43006"
        "tls://[2a12:5940:1464::2]:65534"
        "tls://[2a12:5940:b1a0::2]:65534"
        "tls://37.205.14.171:993"
        "tls://44.234.134.124:443"
        "tls://45.147.200.202:443"
        "tls://45.95.202.21:443"
        "tls://45.95.202.91:65534"
        "tls://51.15.204.214:54321"
        "tls://51.255.223.60:54232"
        "tls://51.38.64.12:28395"
        "tls://54.37.137.221:11129"
        "tls://62.210.85.80:39575"
        "tls://65.21.57.122:61995"
        "tls://77.244.252.40:65534"
        "tls://77.37.218.131:443"
        "tls://77.91.84.76:65534"
        "tls://78.27.153.163:179"
        "tls://78.27.153.163:33166"
        "tls://78.27.153.163:3784"
        "tls://78.27.153.163:3785"
        "tls://79.137.194.94:65534"
        "tls://87.251.77.39:65534"
        "tls://88.210.3.30:65534"
        "tls://94.140.114.241:4708"
        "tls://95.216.5.243:18836"
        "tls://aurora.devices.waren.io:18836"
        "tls://bode.theender.net:42169?key=f91b909f43829f8b20732b3bcf80cbc4bb078dd47b41638379a078e35984c9a4"
        "tls://ca1.servers.devices.cwinfo.net:58226"
        "tls://cloudberry.fr1.servers.devices.cwinfo.net:54232"
        "tls://corn.chowder.land:443"
        "tls://de-fsn-1.peer.v4.yggdrasil.chaz6.com:4444"
        "tls://ekb.itrus.su:7992"
        "tls://fi1.servers.devices.cwinfo.net:61995"
        "tls://fr2.servers.devices.cwinfo.net:23108"
        "tls://mima.localghost.org:1997"
        "tls://pl1.servers.devices.cwinfo.net:11129"
        "tls://pp1.ygg.sy.sa:8442"
        "tls://s2.i2pd.xyz:39575"
        "tls://s-ams-0.sergeysedoy97.ru:65534"
        "tls://s-ams-1.sergeysedoy97.ru:65534"
        "tls://s-fra-0.sergeysedoy97.ru:65534"
        "tls://sin.yuetau.net:6643"
        "tls://s-kiv-0.sergeysedoy97.ru:65534"
        "tls://s-kzn-0.sergeysedoy97.ru:65534"
        "tls://s-led-0.sergeysedoy97.ru:65534"
        "tls://s-mow-0.sergeysedoy97.ru:65534"
        "tls://s-mow-1.sergeysedoy97.ru:65534"
        "tls://s-mow-3.sergeysedoy97.ru:65534"
        "tls://s-mow-4.sergeysedoy97.ru:65534"
        "tls://s-ovb-0.sergeysedoy97.ru:65534"
        "tls://s-ovb-1.sergeysedoy97.ru:65534"
        "tls://srv.itrus.su:7992"
        "tls://s-sto-0.sergeysedoy97.ru:65534"
        "tls://supergay.network:443"
        "tls://supergay.network:9001"
        "tls://uk1.servers.devices.cwinfo.net:28395"
        "tls://vpn.itrus.su:7992"
        "tls://x-ams-0.sergeysedoy97.ru:65534"
        "tls://x-ams-1.sergeysedoy97.ru:65534"
        "tls://x-fra-0.sergeysedoy97.ru:65534"
        "tls://x-kiv-0.sergeysedoy97.ru:65534"
        "tls://x-kzn-0.sergeysedoy97.ru:65534"
        "tls://x-led-0.sergeysedoy97.ru:65534"
        "tls://x-mow-0.sergeysedoy97.ru:65534"
        "tls://x-mow-1.sergeysedoy97.ru:65534"
        "tls://x-mow-2.sergeysedoy97.ru:65534"
        "tls://x-mow-3.sergeysedoy97.ru:65534"
        "tls://x-sto-0.sergeysedoy97.ru:65534"
        "tls://ygg1.mk16.de:1338?key=0000000087ee9949eeab56bd430ee8f324cad55abf3993ed9b9be63ce693e18a"
        "tls://ygg2.mk16.de:1338?key=000000d80a2d7b3126ea65c8c08fc751088c491a5cdd47eff11c86fa1e4644ae"
        "tls://ygg3.mk16.de:1338?key=000003acdaf2a60e8de2f63c3e63b7e911d02380934f09ee5c83acb758f470c1"
        "tls://ygg4.mk16.de:1338?key=0000147df8daa1cce2ad4b1d4b14c60a4c69a991b2dfde4e00ba7e95c36c530b"
        "tls://ygg.ace.ctrl-c.liu.se:9999?key=5636b3af4738c3998284c4805d91209cab38921159c66a6f359f3f692af1c908"
        "tls://yggdrasil-2.herronjo.com:1337?key=6cbcd23d94c9a300e442bd1054c7ced8d09dbb6349261651b24e76851efb7edf"
        "tls://yggdrasil.community.garage.networks.deavmi.assigned.network:2001"
        "tls://yggdrasil.neilalexander.dev:64648"
        "tls://ygg.iva.bz:50002"
        "tls://ygg.jjolly.dev:3443"
        "tls://ygg.mkg20001.io:443"
        "tls://ygg.mnpnk.com:443"
        "tls://ygg-msk-1.averyan.ru:8362"
        "tls://yggno.de:18227"
        "tls://ygg.yt:443"
      ];
    };

  };
}
