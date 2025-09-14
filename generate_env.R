# This script uses the 'rix' package to generate a reproducible Nix environment for an R project.
# The generated 'default.nix' file defines all the necessary R packages and system dependencies.
# The 'rix' package allows for the creation of a self-contained and isolated development environment.

# Load the 'rix' package.
library(rix)

# Call the 'rix' function to configure and generate the Nix environment files.
rix(
  # Relatively fresh packages and have a stable environment for production
  r_ver = "latest-upstream",
  
  # R Packages
  r_pkgs = c(
    # Data manipulation & structures
    "data.table",    # High-performance data manipulation
    "lubridate",     # To work with dates and datetimes
    "stringr",       # To work with the text
    "tibble",        # Modern data frames
    "timeDate",      # Financial time/date handling
    
    # Import/export & storage
    "fst",           # Fast serialization of data
    "qs2",           # High-performance serialization
    "rvest",         # Web scraping
    "here",          # Reproducible file paths

    # Tools for creating maps
    "osmdata",       # Import data from Open Street Map
    "tidycensus",    # Import socioeconomic data from the US Census Bureau
    "units",         # Tools to manage unit conversion
    "leaflet",       # Interactive maps
    "maptiles",      # Basemap tiles for mapping
    "sf",            # Spatial data handling
    "tmap",          # Thematic maps

    # To train ML
    "tidymodels",    # Framework to create models with good practices
    "baguette",      # Extending tidymodels
    "rules",         # Extending tidymodels
    "corrr",         # To explore correlations
    "mixOmics",      # Integrative data analysis
    "glmnet",        # Regularized generalized linear models
    "kernlab",       # Kernel-based machine learning
    "ranger",        # Fast implementation of random forests
    "xgboost",       # Gradient boosting framework
    
    # Databases
    "DBI",           # Database interface
    "duckdb",        # Embedded analytics database

    # To work and plot graphs
    "tidytext",      # Tools for extracting insight from text
    "stringdist",    # To compute string distance
    "hashr",         # To transform text into text hash
    "igraph",        # Tools for working with the text graphs
    
    # String & utilities
    "glue",          # String interpolation
    "withr",         # Scoped environment management
    
    # Visualization
    "ggplot2",       # Core plotting system
    "scales",        # Scales for ggplot2
    "ggiraph",       # Interactive ggplot graphics
    "DiagrammeR",    # Graphs and diagrams
    "patchwork",     # Combine ggplot2 plots
    "gt",            # To create html tables
    
    # Modeling & statistics
    "infer",         # Statistical inference
    "recipes",       # Preprocessing for modeling
    
    # Parallelization & performance
    "future",        # Parallel execution
    "future.apply",  # Apply functions in parallel
    "tictoc",        # Timing operations
    
    # Reporting & documentation
    "knitr",         # Dynamic report generation
    "rmarkdown",     # Reproducible documents
    
    # Development & testing
    "devtools",      # Development tools
    "testthat"       # Unit testing
  ),
  
  # System dependencies
  system_pkgs = c(
    "quarto",  # Document publishing system
    "git"
  ),
  
  # Specifies that no specific IDE is required for the environment.
  ide = "none",
  
  # Specifies the project's root directory.
  project_path = ".",
  
  # Specifies whether to overwrite existing 'flake.nix' and 'default.nix' files.
  overwrite = TRUE
)
