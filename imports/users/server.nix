{
  users.users = {
    root = {
      initialHashedPassword =
        "$y$j9T$odRyg2xqJbySHei1UBsw3.$AxuY704CGICLQqKPm3wiV/b7LVOVSMKnV4iqK1KvAk2";
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICkDT9xZLh+lHc6Z60oLZlLjzOcP39B3D7ptV6xSzAhu openpgp:0x464B6BB1"
      ];
    };
    our = {
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDTc3A1qJl/v0Fkm3MgVom6AaYeSHr7GMHMWgYLzCAAPmfmZBEc3YWNTjnwinGHfuTun5F8hIwg1I/Of0wUYKNwH4Fx7fWQfOkOPxdeVLvy5sHVskwEMYeYteG4PPSDPqov+lQ6jYdL7KjlqQn4nLG5jLQsj47/axwBtdE5uS13cGOnyIuIq3O3djIWWOPv2RWEnc/xHHvsISg6e4HNZJr3W0AOcdd5NPk5Mf9BVj45kdR5TpypvPdTdI5jXYSmlousd5V2dNKqreBj7RX3Fap/vSViPM8EEbgFPC1i7hOWlWTMt12baAFFKZwRvjD6kr/FjUbGzh6Yx14NzJM+yFjwla71nbancL9kQr8S3WBF3OVLT26X43PltiVSfOPR7xsVx5pGbaesEuUPB6b394Z0w3zXAuQANwQbJZTDmjyvPvMDlEDwtoq/wQJvzwfi/n1NTimu3yjWvKFYTMPVH5HUQqj7FrG2c8aldAl18Z+dV/Mymky7CGIgHtT/oG99TSk= comment"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICkDT9xZLh+lHc6Z60oLZlLjzOcP39B3D7ptV6xSzAhu openpgp:0x464B6BB1"
      ];
      # for rtorrent to watch new torrents
      createHome = true;
      homeMode = "755";
    };
    yc = {
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICkDT9xZLh+lHc6Z60oLZlLjzOcP39B3D7ptV6xSzAhu openpgp:0x464B6BB1"
      ];
    };
  };
}
