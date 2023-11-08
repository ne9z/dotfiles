{ pkgs }:
pkgs.texlive.combine {
  inherit (pkgs.texlive)
    collection-basic collection-mathscience
    # languages
    collection-langenglish collection-langgerman
    # pdf manipulation tool
    pdfjam # depends on pdfpages, geometry
    # pdfpages and dependencies
    pdfpages eso-pic atbegshi pdflscape
    # century schoolbook
    schola-otf iftex xkeyval unicode-math fontspec realscripts lualatex-math
    tex-gyre tex-gyre-math libertinus-otf libertinus-fonts stix2-otf
    # beamer
    beamer etoolbox hyperref pgf
    # cjk
    #luatexja chinese-jfm
    # bold computer modern
    newcomputermodern fontsetup
    # koma-script for a4 paper
    koma-script xpatch
    # pdf-archive
    pdfx xmpincl xcolor luatex85 pdftexcmds stringenc
    # checks
    chktex lacheck
    # book index
    xindy
    # deal with intervals
    interval mathtool
    # with pdflatex
    mlmodern
    # pictures and tikz
    collection-pictures;
}
