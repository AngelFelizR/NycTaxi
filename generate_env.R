# This script uses the 'rix' package to generate a reproducible Nix environment for an R project.
# The generated 'default.nix' file defines all the necessary R packages and system dependencies.
# The 'rix' package allows for the creation of a self-contained and isolated development environment.

# Load the 'rix' package.
library(rix)

# Call the 'rix' function to configure and generate the Nix environment files.
rix(
  date = "2025-12-02",

  # R Packages
  r_pkgs = c(
    # Data manipulation & structures
    "data.table", # High-performance data manipulation
    "lubridate", # To work with dates and datetimes
    "stringr", # To work with the text
    "tibble", # Modern data frames
    "timeDate", # Financial time/date handling

    # Import/export & storage
    "fst", # Fast serialization of data
    "qs2", # High-performance serialization
    "rvest", # Web scraping
    "here", # Reproducible file paths

    # Tools for creating maps
    "osmdata", # Import data from Open Street Map
    "tidycensus", # Import socioeconomic data from the US Census Bureau
    "units", # Tools to manage unit conversion
    "leaflet", # Interactive maps
    "maptiles", # Basemap tiles for mapping
    "sf", # Spatial data handling
    "tmap", # Thematic maps

    # To train ML
    "tidymodels", # Framework to create models with good practices
    "baguette", # Extending tidymodels
    "glmnet", # Regularized generalized linear models
    "earth", # Multivariate Adaptive Regression Splines machine learning
    "mda", # Mixture and Flexible Discriminant Analysis
    "C50", #C5.0 Decision Trees and Rule-Based Models
    "ranger", # Fast implementation of random forests
    "xgboost", # Gradient boosting framework
    "klaR", # For regularized discriminant analysis
    "discrim", # To fit various discriminant analysis models
    "mixOmics", # Engine for step_pls
    "embed", # To use step_umap
    "themis", # To apply Down-Sample
    "corrr", # To explore correlations

    # Explain Models
    "DALEXtra", # DALEXtra: Extension for 'DALEX' Package
    "vip", # To get Feature Importance based on models

    # Databases
    "DBI", # Database interface
    "duckdb", # Embedded analytics database

    # To work and plot graphs
    "tidytext", # Tools for extracting insight from text
    "stringdist", # To compute string distance
    "hashr", # To transform text into text hash
    "igraph", # Tools for working with the text graphs

    # String & utilities
    "glue", # String interpolation
    "withr", # Scoped environment management

    # Visualization
    "ggplot2", # Core plotting system
    "scales", # Scales for ggplot2
    "ggiraph", # Interactive ggplot graphics
    "DiagrammeR", # Graphs and diagrams
    "patchwork", # Combine ggplot2 plots
    "gt", # To create html tables
    "ggtext", # Exteding more text for ggplot2

    # Modeling & statistics
    "infer", # Statistical inference
    "recipes", # Preprocessing for modeling

    # Parallelization & performance
    "future", # Parallel execution
    "future.apply", # Apply functions in parallel
    "tictoc", # Timing operations

    # Reporting & documentation
    "knitr", # Dynamic report generation
    "rmarkdown", # Reproducible documents

    # Development & testing
    "devtools", # Development tools
    "testthat", # Unit testing
    "roxygen2@7.3.2"
  ),

  # Getting packages from github
  git_pkgs = list(
    list(
      package_name = "corrcat",
      repo_url = "https://github.com/AngelFelizR/corrcat/",
      commit = "d20b6ad6a01d3b5a605037dbffbca3a18e97ac00"
    ),
    list(
      package_name = "pins",
      repo_url = "https://github.com/rstudio/pins-r/",
      commit = "a6de9732c2dded36688b6b27c58bfd2b45854e2a"
    )
  ),

  # System dependencies
  system_pkgs = c(
    "quarto"
  ),

  # Specifies that no specific IDE is required for the environment.
  ide = "none",

  # Specifies the project's root directory.
  project_path = ".",

  # Specifies whether to overwrite existing 'flake.nix' and 'default.nix' files.
  overwrite = TRUE
)
