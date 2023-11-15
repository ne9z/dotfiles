{ pkgs }:
pkgs.texlive.combine {
  inherit (pkgs.texlive)
    collection-basic collection-mathscience
    collection-pictures
    collection-latexrecommended
    collection-fontsrecommended
    collection-luatex

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

    ###### cjk
    luatexja adobemapping chinese-jfm ctex cjk
    ###### cjk fonts
    fandol arphic-ttf arphic gsftopk
    collection-langcjk collection-langchinese

    ###### preview
    dvipng

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
    # luatex support for pdftex commands
    luatex85 pdftexcmds
    # pdfx for PDF/A compliance
    pdfx xmpincl xcolor stringenc
    # beamer
    beamer etoolbox hyperref pgf
    # checks
    chktex lacheck;
}
