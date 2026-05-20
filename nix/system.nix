let
  pkgs = import ./pkgs.nix;
in
  pkgs.buildEnv {
    name = "system-packages";
    paths = builtins.attrValues {
      inherit (pkgs)
        glibcLocales
        nix
        R
        quarto
        which
        pandoc
        fontconfig
        dejavu_fonts
        freefont_ttf;
    };
  }
