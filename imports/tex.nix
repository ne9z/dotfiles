{ pkgs }:
pkgs.texlive.combine {
  inherit (pkgs.texlive)
    collection-basic collection-mathscience collection-luatex collection-latex
    collection-latexrecommended collection-pictures collection-fontsrecommended

    # languages
    collection-langenglish collection-langgerman

    # deal with intervals
    interval mathtools

    ####### fonts
    # with pdflatex
    mlmodern
    # otf fonts
    tex-gyre tex-gyre-math libertinus-fonts stix2-otf
    #######

    ###### pdf manipulation tool
    pdfjam # depends on pdfpages, geometry
    # pdfpages and dependencies
    pdfpages eso-pic atbegshi pdflscape
    ######

    ###### misc
    # unicode-math
    lualatex-math
    # fonts for unicode-math
    schola-otf libertinus-otf iftex xkeyval unicode-math fontspec realscripts
    luatex85
    # beamer
    beamer etoolbox hyperref pgf
    # checks
    chktex lacheck;
}
