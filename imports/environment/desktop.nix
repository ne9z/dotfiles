{ pkgs, ... }:
{
  environment = {
    systemPackages = builtins.attrValues {
      inherit (pkgs) virt-manager;
      inherit (pkgs) poppler perl;
      inherit (pkgs)
      # for use with emacs preview-latex
        ghostscript
        # spell checkers
        hunspell
        # used with dired mode to open files
        xdg-utils;
      inherit (pkgs.hunspellDicts) en_US de_DE;
      inherit emacsPkg mytex;
    };
    interactiveShellInit = ''
      e () { $EDITOR --create-frame "$@"; }
    '';
    sessionVariables = {
      VAAPI_DISABLE_INTERLACE = "1";
      W3M_DIR = "$HOME/.config/w3m";
      XCURSOR_THEME = "Adwaita";
      XCURSOR_SIZE = "16";
    };
  };
}
