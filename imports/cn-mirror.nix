{ lib, ... }: {
  nix.settings.substituters =
    lib.mkBefore [ "https://mirror.sjtu.edu.cn/nix-channels/store" ];
}
