let
  pkgs = import ./pkgs.nix;

  corrcat = pkgs.rPackages.buildRPackage {
    name = "corrcat";
    src = pkgs.fetchgit {
      url = "https://github.com/AngelFelizR/corrcat/";
      rev = "d20b6ad6a01d3b5a605037dbffbca3a18e97ac00";
      sha256 = "sha256-MT+SEkz/6VRIc9JairTCt0gEmxLGpgPH4eevlFqPUyA=";
    };
    propagatedBuildInputs = builtins.attrValues {
      inherit (pkgs.rPackages)
        data_table
        rcompanion
        tibble;
    };
  };

  pins = pkgs.rPackages.buildRPackage {
    name = "pins";
    src = pkgs.fetchgit {
      url = "https://github.com/rstudio/pins-r/";
      rev = "a6de9732c2dded36688b6b27c58bfd2b45854e2a";
      sha256 = "sha256-IJg/dGXZtRfDc9F3AgkMF/wr49q8utwGo2sGZeYjVZ4=";
    };
    propagatedBuildInputs = builtins.attrValues {
      inherit (pkgs.rPackages)
        cli
        digest
        fs
        generics
        glue
        httr
        jsonlite
        lifecycle
        purrr
        rappdirs
        rlang
        tibble
        whisker
        withr
        yaml;
    };
  };

  roxygen2 = pkgs.rPackages.buildRPackage {
    name = "roxygen2";
    src = pkgs.fetchzip {
      url = "https://cran.r-project.org/src/contrib/Archive/roxygen2/roxygen2_7.3.2.tar.gz";
      sha256 = "sha256-yN5dy5xp76jQeKBku0e8sjhizBG7Q9k3HjZLtjwVrnE=";
    };
    propagatedBuildInputs = builtins.attrValues {
      inherit (pkgs.rPackages)
        brew
        cli
        commonmark
        desc
        knitr
        pkgload
        purrr
        R6
        rlang
        stringi
        stringr
        withr
        xml2
        cpp11;
    };
  };

in
  pkgs.buildEnv {
    name = "r-custom";
    paths = [ corrcat pins roxygen2 ];
  }
