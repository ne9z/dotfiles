{ pkgs }:
pkgs.texlive.combine {
  inherit (pkgs.texlive)
    collection-basic
    collection-mathscience
    collection-context
    collection-luatex
    collection-latex
    collection-latexrecommended
    collection-pictures
    collection-fontsrecommended

    # languages
    collection-langenglish
    collection-langgerman

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
    # beamer
    beamer etoolbox hyperref pgf
    # pdf-archive
    pdfx xmpincl xcolor luatex85 pdftexcmds stringenc
    # checks
    chktex lacheck;
}
