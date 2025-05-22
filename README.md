# Increasing NYC Taxi Drivers Earnings


## Problem description

**Opportunity**

Taxi drivers could **increase their earnings** by changing their
strategy.

**Questions to solve**

- How much a taxi driver can increase its monthly earning just by
  **skipping trips under defined conditions**?
- How much a taxi driver can increase its monthly earning just by
  **changing its initial zone and time**?

**Business success criteria**

Develop a strategy to **increase** NYC taxi drivers’ monthly earnings
**by 20%**.

**Project scope**

This project will be limited to **Juno**, **Uber**, **Via** and **Lyft**
taxi drivers who work in New York City in trips that take place between
any zone of **Manhattan**, **Brooklyn** or **Queens** (the more active
ones).

## Metodology

To solve those questions we are going to use use the **Cross-Industry
Standard Process for Data Mining** (CRISP-DM).

<img src="figures/CRISP-DM_Process_Diagram.png" style="display: block; margin-left: auto; margin-right: auto; width: 50%;">

And based on this steps we can organize the articles created in this
portfolio web site.

1.  **Business Understanding**

    1.  Business Understanding Overview
    2.  Defining Base Line

2.  **Data Understanding**

    3.  Defining Development Environment
    4.  Data Collection Process
    5.  Data Sampling
    6.  Initial Exploration
    7.  Expanding Geospatial Information
    8.  Exploring Transportation and Socioeconomic Patterns

3.  **Data Preparation**

    - *(Pending)*

4.  **Modeling**

    - *(Pending)*

5.  **Evaluation**

    - *(Pending)*

6.  **Deployment**

    - *(Pending)*

## Data to use

In this project, we will use a subset of the data available in the [TLC
Trip Record
Data](https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page) from
2022 to 2023 for **High Volume For-Hire Vehicle** with the columns
described in its [data
dictionary](https://www.nyc.gov/assets/tlc/downloads/pdf/data_dictionary_trip_records_hvfhs.pdf).

## Results highlight

*(Pending)*

## Tech stack

**Core Language:** R

**Key Ecosystems & Frameworks:**

- **Tidyverse:** Heavily utilized, including core packages like:
  - `dplyr` (Data Manipulation)
  - `ggplot2` (Data Visualization)
  - `tidyr` (Data Tidying)
  - `readr` (Data Import)
  - `purrr` (Functional Programming)
  - `stringr` (String Manipulation)
  - `lubridate` (Dates/Times)
  - `forcats` (Factor Handling)
  - `tibble` (Modern Data Frames)
- **Geospatial Analysis:** Extensive use of spatial packages:
  - `sf` (Simple Features - Modern standard for spatial data)
  - `leaflet` & `tmap` (Interactive and static thematic mapping)
  - `terra` & `raster` (Raster data processing)
  - `osmdata` (OpenStreetMap data access)
  - Supporting spatial packages (`sp`, `lwgeom`, `s2`, `units`, `proj4`,
    `wk`, etc.)
- **Modeling & Preprocessing:**
  - `recipes` (Data preprocessing pipelines for modeling)
  - `rpart` (Decision Trees)
  - Potentially others depending on usage (`MASS`, `nnet`, `e1071`,
    `ipred`, `correlationfunnel`)
  - `broom` (Tidying model outputs)
  - `infer` (Statistical inference)
- **Data Handling & Access:**
  - `data.table` (High-performance data manipulation)
  - `DBI` & `duckdb` (Database connectivity and in-process analytics
    database)
  - `httr`, `httr2`, `curl`, `rvest` (Web data access and scraping)
  - `fst`, `qs2` (Fast data serialization)
  - `vroom`, `readxl` (Data import)
- **Reporting, Visualization & Apps:**
  - `rmarkdown` & `knitr` (Report generation)
  - `shiny` (Interactive web applications)
  - `plotly` (Interactive plots)
  - Various HTML widget-based visualization packages (`DiagrammeR`,
    `networkD3`, `visNetwork`, `ggiraph`)
- **Workflow & Parallel Processing:**
  - `renv` (Project environment management)
  - `here` (Project path management)
  - `future`, `future.apply`, `parallelly` (Parallel and asynchronous
    processing)

## Disclaimer

This project was completed by making strong assumptions due the reality
that the data used to create the analysis don’t provide any unique
identifier for taxi drivers, that could help us to deliver more
realistic results.

On the other hand, this project aims to increase **taxi driver
earnings**, regardless that if we apply it extensively, it could also
end producing the following results:

1.  **Reduced service quality:** Drivers focusing solely on maximizing
    earnings may avoid less profitable areas or times, potentially
    leaving some passengers underserved.

2.  **Increased congestion:** Drivers congregating in high-profit areas
    could worsen traffic in already busy parts of the city.

In conclusion, this project was created to show my abilities as Data
Scientist, but it is not a project that should be implemented due this
considerations.
