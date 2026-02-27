let
 pkgs = import (fetchTarball "https://github.com/rstats-on-nix/nixpkgs/archive/2025-12-02.tar.gz") {};
 
  rpkgs = builtins.attrValues {
    inherit (pkgs.rPackages) 
      baguette
      C50
      corrr
      DALEXtra
      data_table
      DBI
      devtools
      DiagrammeR
      discrim
      duckdb
      earth
      embed
      fst
      future
      future_apply
      fuzzyjoin
      ggiraph
      ggplot2
      ggtext
      glmnet
      glue
      gt
      hashr
      here
      igraph
      infer
      klaR
      knitr
      leaflet
      lubridate
      maptiles
      mda
      mixOmics
      osmdata
      patchwork
      plotly
      qs2
      ranger
      recipes
      rmarkdown
      rvest
      scales
      sf
      stringdist
      stringr
      testthat
      themis
      tibble
      tictoc
      tidycensus
      tidymodels
      tidytext
      timeDate
      tmap
      units
      vip
      withr
      xgboost;
  };
 
    corrcat = (pkgs.rPackages.buildRPackage {
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
    });

    pins = (pkgs.rPackages.buildRPackage {
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
    });
 
    roxygen2 = (pkgs.rPackages.buildRPackage {
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
    });

  system_packages = builtins.attrValues {
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

  shell = pkgs.mkShell {
    LOCALE_ARCHIVE = if pkgs.system == "x86_64-linux" then "${pkgs.glibcLocales}/lib/locale/locale-archive" else "";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";

    # Font configuration for ggplot2/ggtext
    FONTCONFIG_FILE = "${pkgs.fontconfig.out}/etc/fonts/fonts.conf";
    FONTCONFIG_PATH = "${pkgs.fontconfig.out}/etc/fonts/";

    # Tell fontconfig where to find the fonts from the Nix store
    shellHook = ''
      export XDG_DATA_DIRS="${pkgs.dejavu_fonts}/share:${pkgs.freefont_ttf}/share:$XDG_DATA_DIRS"
      fc-cache -f 2>/dev/null || true
    '';
  
    buildInputs = [ corrcat pins roxygen2 rpkgs system_packages ];
    
  }; 
in
  {
    inherit pkgs shell;
  }
