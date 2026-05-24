let
  pkgs = import ./pkgs.nix;
in
  pkgs.buildEnv {
    name = "r-ml";
    paths = builtins.attrValues {
      inherit (pkgs.rPackages)
        # Tree models
        xgboost
        ranger
        C50
        baguette
        rpart_plot
        # Linear models
        glmnet
        earth
        mda
        # Other models
        discrim
        klaR
        # Explainability
        DALEXtra
        vip
        # Feature engineering
        embed
        themis
        # Ensembles / stacking
        mixOmics;
    };
  }
