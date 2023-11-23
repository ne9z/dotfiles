# configuration in this file is shared by all hosts

{ pkgs, pkgs-unstable, inputs, lib, ... }:
let
  inherit (inputs) self;
  inherit (lib) mkDefault;
in {
  # Safety mechanism: refuse to build unless everything is
  # tracked by git
  system.configurationRevision = if (self ? rev) then
    self.rev
  else
    throw "refuse to build: git tree is dirty";
}
