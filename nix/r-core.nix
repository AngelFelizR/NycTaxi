let
  pkgs = import ./pkgs.nix;
in
  pkgs.buildEnv {
    name = "r-core";
    paths = builtins.attrValues {
      inherit (pkgs.rPackages)
        # Tidyverse core
        tibble
        stringr
        glue
        lubridate
        patchwork
        scales
        # Tidymodels
        tidymodels
        recipes
        infer
        # Reporting
        knitr
        rmarkdown
        ggplot2
        ggtext
        ggiraph
        GGally
        plotly
        gt
        # Utilities
        here
        withr
        tictoc
        testthat
        devtools;
    };
  }
