let
  pkgs = import ./pkgs.nix;
in
  pkgs.buildEnv {
    name = "r-geo";
    paths = builtins.attrValues {
      inherit (pkgs.rPackages)
        sf
        tmap
        maptiles
        osmdata
        leaflet
        tidycensus
        units;
    };
  }
