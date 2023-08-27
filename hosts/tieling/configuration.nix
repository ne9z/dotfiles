{ ... }: {
  networking = {
    hostName = "tieling";
    hostId = "38bdc3d4";
  };
  time.timeZone = "Asia/Shanghai";
  imports = [ ../imports/common.nix ../imports/server.nix ];
}
