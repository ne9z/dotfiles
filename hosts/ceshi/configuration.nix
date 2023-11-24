{ pkgs, ... }: {
  imports = [ ../../imports/profiles/live.nix ];
  time.timeZone = "Asia/Shanghai";
  networking = { hostName = "ceshi"; };
  # prohibit javascript
  programs.chromium.extraOpts."DefaultJavaScriptSetting" = 2;
}
