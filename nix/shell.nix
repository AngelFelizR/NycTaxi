let
  pkgs = import ./pkgs.nix;

  # Reference each pre-built environment by path.
  # nix-build produces a symlink in ./result; here we import
  # each layer's derivation directly so the shell can wire them up.
  system  = import ./system.nix;
  rCore   = import ./r-core.nix;
  rMl     = import ./r-ml.nix;
  rGeo    = import ./r-geo.nix;
  rData   = import ./r-data.nix;
  rCustom = import ./r-custom.nix;

in
  pkgs.mkShell {
    LOCALE_ARCHIVE = if pkgs.system == "x86_64-linux"
                     then "${pkgs.glibcLocales}/lib/locale/locale-archive"
                     else "";
    LANG            = "en_US.UTF-8";
    LC_ALL          = "en_US.UTF-8";
    LC_TIME         = "en_US.UTF-8";
    LC_MONETARY     = "en_US.UTF-8";
    LC_PAPER        = "en_US.UTF-8";
    LC_MEASUREMENT  = "en_US.UTF-8";

    FONTCONFIG_FILE = "${pkgs.fontconfig.out}/etc/fonts/fonts.conf";
    FONTCONFIG_PATH = "${pkgs.fontconfig.out}/etc/fonts/";

    shellHook = ''
      export XDG_DATA_DIRS="${pkgs.dejavu_fonts}/share:${pkgs.freefont_ttf}/share:$XDG_DATA_DIRS"
      fc-cache -f 2>/dev/null || true
    '';

    buildInputs = [ system rCore rMl rGeo rData rCustom ];
  }
