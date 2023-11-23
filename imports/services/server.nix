{
  services = {
    zfs = {
      autoScrub = {
        enable = true;
        interval = "quarterly";
      };
      # autoSnapshot enabled in common.nix
    };
    nfs = {
      server = {
        enable = true;
        createMountPoints = true;
        exports = ''
          /tmp/BitTorrent    192.168.1.0/24(ro,all_squash)
        '';
        # disable nfs3
        extraNfsdConfig = ''
          vers2=n
          vers3=n
        '';
      };
    };
    samba = {
      enable = true;
      openFirewall = true;
      extraConfig = ''
        guest account = nobody
        map to guest = bad user
        server smb encrypt = off
      '';
      shares = {
        bt = {
          path = "/tmp/BitTorrent";
          "read only" = true;
          browseable = "yes";
          "guest ok" = "yes";
          "hosts allow" = "192.168.1.";
        };
      };
    };
    rtorrent = {
      enable = true;
      dataDir = "/tmp/BitTorrent";
      downloadDir = "/tmp/BitTorrent/已下载";
      openFirewall = false;
      dataPermissions = "0755";
      configText = ''
        # rtorrent program settings
        encoding.add = UTF-8
        pieces.hash.on_completion.set = 0
        system.umask.set = 0022

        # torrent network settings
        network.port_range.set = 51413-51413
        dht.mode.set = on
        protocol.pex.set = yes
        trackers.use_udp.set = yes
        protocol.encryption.set = none

        # watch dir
        # created and permission set by systemd tmpdir rules
        method.insert = cfg.watchDir1, private|const|string, "/home/our/新种子/"
        # Watch directories (add more as you like, but use unique schedule names)
        schedule2 = watch_start, 10, 10, ((load.start, (cat, (cfg.watchDir1), "/*.torrent")))

        # disable prealloc for zfs
        system.file.allocate.set = 0

        # performance tuning
        # https://github.com/rakshasa/rtorrent/issues/1046
        ### BitTorrent
        # Global upload and download rate in KiB, `0` for unlimited
        throttle.global_down.max_rate.set = 0
        throttle.global_up.max_rate.set = 0

        # Maximum number of simultaneous downloads and uploads slots
        throttle.max_downloads.global.set = 150
        throttle.max_uploads.global.set = 300

        # Maximum and minimum number of peers to connect to per torrent while downloading
        throttle.min_peers.normal.set = 30
        throttle.max_peers.normal.set = 150

        # Same as above but for seeding completed torrents (seeds per torrent)
        throttle.min_peers.seed.set = -1
        throttle.max_peers.seed.set = -1

        network.max_open_files.set = 4096
        network.max_open_sockets.set = 1536
        network.http.max_open.set = 48
        network.send_buffer.size.set = 128M
        network.receive_buffer.size.set = 4M

        ### Memory Settings
        pieces.hash.on_completion.set = no
        pieces.preload.type.set = 1

        ### add more torrents at once
        network.xmlrpc.size_limit.set = 5M
      '';
    };
    openssh = {
      ports = [ 22 65222 ];
      allowSFTP = true;
      openFirewall = true;
    };
    yggdrasil.persistentKeys = true;
    tor = {
      enable = true;
      relay = {
        enable = false;
        onionServices = {
          ssh = {
            authorizedClients = [ ];
            map = [{
              port = 22;
              target = {
                addr = "[::1]";
                port = 22;
              };
            }];
          };
        };
      };
      settings = {
        ClientUseIPv6 = true;
        ClientPreferIPv6ORPort = true;
        ClientUseIPv4 = true;
        UseBridges = 1;
        Bridge = [
          # see https://bridges.torproject.org/bridges/?transport=0&ipv6=yes
          # no javascript needed on that page
          "[2a03:4000:65:b0a:541c:a0ff:fec1:6737]:9010 B896373B1049FC39797A690B6A86503DB768150F"
          "[2001:41d0:800:60d::ff]:9001 59F622BDF6888CE480038F899CBCC2B7438B19A3"
        ];
      };
    };
    i2pd = {
      enable = true;
      inTunnels = {
        ssh-server = {
          enable = true;
          address = "::1";
          destination = "::1";
          #keys = "‹name›-keys.dat";
          #key is generated if missing
          port = 65222;
          accessList = [ ]; # to lazy to only allow zfs-root laptops
        };
      };
    };
  };
}
