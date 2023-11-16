{ pkgs }:
pkgs.texlive.combine {
  inherit (pkgs.texlive)
    collection-basic collection-mathscience collection-pictures
    collection-latexrecommended collection-fontsrecommended collection-luatex

    # languages
    collection-langenglish collection-langgerman

    # deal with intervals
    interval

    # non unicode-math: enhanced amsmath support
    mathtools

    ####### fonts
    # type1-font only: bold computer modern
    # use with \usepackage[T1]{fontenc}
    # \usepackage{mlmodern}
    mlmodern

    # otf fonts
    tex-gyre tex-gyre-math libertinus-fonts stix2-otf
    #######

    ###### cjk
    # cjk for luatex; chinese included
    luatexja
    # luatexja deps
    adobemapping chinese-jfm haranoaji
    # CTEX 中文科技排版 <- use this
    ctex
    # only suitable for short Chinese text
    cjk
    ###### cjk fonts
    fandol arphic-ttf arphic
    ###### chinese language collection
    collection-langcjk collection-langchinese

    ###### pdftex only: preview
    # luatex does not support dvi output
    dvipng
    # preamble cache
    mylatex

    ###### pdf manipulation tool
    pdfjam # depends on pdfpages, geometry
    # pdfpages and dependencies
    pdfpages eso-pic atbegshi pdflscape
    ######

    ###### misc
    # unicode-math and deps
    unicode-math fontspec realscripts lualatex-math
    # fonts for unicode-math
    schola-otf libertinus-otf iftex xkeyval
    # luatex support for pdftex commands
    luatex85 pdftexcmds
    # pdfx for PDF/A compliance
    pdfx xmpincl xcolor stringenc
    # beamer
    beamer etoolbox hyperref pgf
    # checks
    chktex lacheck;
}
