{
  networking.firewall = {
    allowedTCPPorts = [
      # bt
      51413
      # nfsv4
      2049
    ];
    allowedUDPPorts = [
      # bt
      51413
    ];
  };
}
