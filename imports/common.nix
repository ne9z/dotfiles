{ config, pkgs, lib, inputs, modulesPath, ... }:
let
  inherit (lib) mkMerge mapAttrsToList mkDefault;
  inherit (inputs) self nixpkgs;
in {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/hardened.nix")
  ];
  boot.initrd.systemd.enable = true;
  # workaround for hardened profile
  services.logrotate.checkConfig = false;

  environment.etc."wpa_supplicant.conf".text = "#";

  networking = {
    useDHCP = true;
    useNetworkd = true;
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

  services.tlp = { enable = true; };

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
        "tcp://158.101.229.219:17002"
        "tcp://185.165.169.234:8880"
        "tcp://188.225.9.167:18226"
        "tcp://193.107.20.230:7743"
        "tcp://193.111.114.28:8080"
        "tcp://195.123.245.146:7743"
        "tcp://[2603:c023:8001:1600:35e0:acde:2c6e:b27f]:17002"
        "tcp://[2a00:b700::a:279]:12402"
        "tcp://[2a09:5302:ffff::992]:12403"
        "tcp://45.147.200.202:12402"
        "tcp://45.95.202.21:12403"
        "tcp://51.15.204.214:12345"
        "tcp://77.37.218.131:12402"
        "tcp://78.27.153.163:33165"
        "tcp://80.78.27.103:30111"
        "tcp://94.130.203.208:5999"
        "tcp://box.paulll.cc:13337"
        "tcp://corn.chowder.land:9002"
        "tcp://cowboy.supergay.network:9111"
        "tcp://ekb.itrus.su:7991"
        "tcp://gutsche.tech:8888"
        "tcp://kusoneko.moe:9002"
        "tcp://longseason.1200bps.xyz:13121"
        "tcp://phrl42.ydns.eu:8842"
        "tcp://sin.yuetau.net:6642"
        "tcp://srv.itrus.su:7991"
        "tcp://supergay.network:9002"
        "tcp://vpn.itrus.su:7991"
        "tcp://ygg1.mk16.de:1337?key=0000000087ee9949eeab56bd430ee8f324cad55abf3993ed9b9be63ce693e18a"
        "tcp://ygg2.mk16.de:1337?key=000000d80a2d7b3126ea65c8c08fc751088c491a5cdd47eff11c86fa1e4644ae"
        "tcp://ygg3.mk16.de:1337?key=000003acdaf2a60e8de2f63c3e63b7e911d02380934f09ee5c83acb758f470c1"
        "tcp://ygg4.mk16.de:1337?key=0000147df8daa1cce2ad4b1d4b14c60a4c69a991b2dfde4e00ba7e95c36c530b"
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
        "tls://102.223.180.74:993"
        "tls://152.228.216.112:23108"
        "tls://158.101.229.219:17001"
        "tls://165.227.17.198:2235"
        "tls://167.160.89.98:7040"
        "tls://178.20.41.3:65535"
        "tls://185.103.109.63:65535"
        "tls://185.130.44.194:7040"
        "tls://185.165.169.234:8443"
        "tls://185.175.90.87:43006"
        "tls://188.225.9.167:18227"
        "tls://192.99.145.61:58226"
        "tls://193.111.114.28:1443"
        "tls://[2001:41d0:303:13b3:51:255:223:60]:54232"
        "tls://[2001:41d0:304:200::ace3]:23108"
        "tls://[2001:41d0:601:1100::cf2]:11129"
        "tls://[2001:41d0:801:2000::233f]:28395"
        "tls://23.137.249.65:444"
        "tls://23.137.251.45:5222"
        "tls://[2603:c023:8001:1600:35e0:acde:2c6e:b27f]:17001"
        "tls://[2605:9f80:2000:64::2]:7040"
        "tls://[2607:5300:201:3100::50a1]:58226"
        "tls://[2a00:b700:2::5:2d4]:65535"
        "tls://[2a00:b700:3::3:3b4]:65535"
        "tls://[2a00:b700:4::3:6f]:65535"
        "tls://[2a00:b700:5::5:34]:65535"
        "tls://[2a00:b700::a:279]:443"
        "tls://[2a00:b700::c:2e1]:65535"
        "tls://[2a01:4f9:2a:60c::2]:18836"
        "tls://[2a01:4f9:c010:664d::1]:61995"
        "tls://[2a03:3b40:fe:ab::1]:993"
        "tls://[2a07:e01:105:444:c634:6bff:feb5:6e28]:7040"
        "tls://[2a09:5302:ffff::992]:443"
        "tls://[2a09:5302:ffff::ac9]:65535"
        "tls://[2a09:5302:ffff::aca]:65535"
        "tls://[2a0d:8480:1:2e::]:65535"
        "tls://[2a0d:8480:2:6bd::]:65535"
        "tls://[2a10:4740:40:0:2222:3f9c:b7cf:1]:43006"
        "tls://[2a12:5940:1464::2]:65535"
        "tls://[2a12:5940:b1a0::2]:65535"
        "tls://37.205.14.171:993"
        "tls://44.234.134.124:443"
        "tls://45.147.198.155:6010"
        "tls://45.147.200.202:443"
        "tls://45.95.202.21:443"
        "tls://45.95.202.91:65535"
        "tls://51.15.204.214:54321"
        "tls://51.255.223.60:54232"
        "tls://51.38.64.12:28395"
        "tls://54.37.137.221:11129"
        "tls://65.21.57.122:61995"
        "tls://77.244.252.40:65534"
        "tls://77.37.218.131:443"
        "tls://77.91.84.76:65535"
        "tls://78.27.153.163:179"
        "tls://78.27.153.163:33166"
        "tls://78.27.153.163:3784"
        "tls://78.27.153.163:3785"
        "tls://79.137.194.94:65535"
        "tls://87.251.77.39:65535"
        "tls://88.210.3.30:65535"
        "tls://94.103.82.150:8080"
        "tls://94.140.114.241:4708"
        "tls://95.216.5.243:18836"
        "tls://aurora.devices.waren.io:18836"
        "tls://box.paulll.cc:13338"
        "tls://ca1.servers.devices.cwinfo.net:58226"
        "tls://cloudberry.fr1.servers.devices.cwinfo.net:54232"
        "tls://corn.chowder.land:443"
        "tls://de-fsn-1.peer.v4.yggdrasil.chaz6.com:4444"
        "tls://ekb.itrus.su:7992"
        "tls://fi1.servers.devices.cwinfo.net:61995"
        "tls://fr2.servers.devices.cwinfo.net:23108"
        "tls://gutsche.tech:8889"
        "tls://longseason.1200bps.xyz:13122"
        "tls://pl1.servers.devices.cwinfo.net:11129"
        "tls://s-ams-0.sergeysedoy97.ru:65535"
        "tls://s-ams-1.sergeysedoy97.ru:65535"
        "tls://s-fra-0.sergeysedoy97.ru:65535"
        "tls://sin.yuetau.net:6643"
        "tls://s-kzn-0.sergeysedoy97.ru:65535"
        "tls://s-led-0.sergeysedoy97.ru:65535"
        "tls://s-mow-0.sergeysedoy97.ru:65535"
        "tls://s-mow-1.sergeysedoy97.ru:65535"
        "tls://s-mow-2.sergeysedoy97.ru:65535"
        "tls://s-mow-3.sergeysedoy97.ru:65535"
        "tls://s-mow-4.sergeysedoy97.ru:65535"
        "tls://s-ovb-0.sergeysedoy97.ru:65535"
        "tls://s-ovb-1.sergeysedoy97.ru:65535"
        "tls://srv.itrus.su:7992"
        "tls://s-sto-0.sergeysedoy97.ru:65535"
        "tls://supergay.network:443"
        "tls://supergay.network:9001"
        "tls://uk1.servers.devices.cwinfo.net:28395"
        "tls://vpn.itrus.su:7992"
        "tls://vpn.ltha.de:443?key=0000006149970f245e6cec43664bce203f2514b60a153e194f31e2b229a1339d"
        "tls://x-ams-0.sergeysedoy97.ru:65535"
        "tls://x-ams-1.sergeysedoy97.ru:65535"
        "tls://x-fra-0.sergeysedoy97.ru:65535"
        "tls://x-led-0.sergeysedoy97.ru:65535"
        "tls://x-mow-0.sergeysedoy97.ru:65535"
        "tls://x-mow-1.sergeysedoy97.ru:65535"
        "tls://x-mow-2.sergeysedoy97.ru:65535"
        "tls://x-mow-3.sergeysedoy97.ru:65535"
        "tls://x-mow-4.sergeysedoy97.ru:65535"
        "tls://x-ovb-0.sergeysedoy97.ru:65535"
        "tls://x-ovb-1.sergeysedoy97.ru:65535"
        "tls://x-sto-0.sergeysedoy97.ru:65535"
        "tls://ygg1.mk16.de:1338?key=0000000087ee9949eeab56bd430ee8f324cad55abf3993ed9b9be63ce693e18a"
        "tls://ygg2.mk16.de:1338?key=000000d80a2d7b3126ea65c8c08fc751088c491a5cdd47eff11c86fa1e4644ae"
        "tls://ygg3.mk16.de:1338?key=000003acdaf2a60e8de2f63c3e63b7e911d02380934f09ee5c83acb758f470c1"
        "tls://ygg4.mk16.de:1338?key=0000147df8daa1cce2ad4b1d4b14c60a4c69a991b2dfde4e00ba7e95c36c530b"
        "tls://yggdrasil.community.garage.networks.deavmi.assigned.network:2001"
        "tls://yggdrasil.su:62586"
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
