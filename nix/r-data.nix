let
  pkgs = import ./pkgs.nix;
in
  pkgs.buildEnv {
    name = "r-data";
    paths = builtins.attrValues {
      inherit (pkgs.rPackages)
        # Data formats
        data_table
        fst
        qs2
        DBI
        duckdb
        # Web / scraping
        rvest
        # Text / NLP
        tidytext
        stringdist
        fuzzyjoin
        # Graphs
        igraph
        DiagrammeR
        # Parallelism
        future
        future_apply
        # Misc
        corrr
        hashr
        timeDate;
    };
  }
