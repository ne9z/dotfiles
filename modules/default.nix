{ config, lib, pkgs, ... }: {
  imports = [ ./boot ./users ./fileSystems ./networking ./per-user ./programs ];
}
