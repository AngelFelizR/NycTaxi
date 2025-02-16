
# Loading Packages ----

## Custom functions
library('project.nyc.taxi')

## To transform data that fits in RAM
library(data.table)
library(lubridate)
library(future)

## To import and export data frames as binary files
library(fst)

## To manage relative paths
library(here)


# Importing data ----

PointMeanDistance <-
  here("output/cache-data/PointMeanDistance.fst") |>
  read_fst(as.data.table = TRUE)

ValidZoneSample <-
  here("output/cache-data/ValidZoneSample.fst") |>
  read_fst(as.data.table = TRUE)


# Selecting the data to use -----

ValidZoneSampleByMonth <-
  
  ## Excluding trips that took place in the last 15 min minutes
  ## to be able to run this process month by month as solution
  ## to avoid running the process on disk which can be really slow
  ValidZoneSample[, request_datetime_extra := request_datetime + minutes(15)
  ][floor_date(request_datetime_extra, unit = "month") == floor_date(request_datetime, unit = "month"),
    .(data = list(.SD)), 
    keyby = c("year", "month"),
    .SDcols = !c("request_datetime_extra")
    
  ## Adding parquet files for each month
  ][, source_path := dir(here("raw-data/trip-data"), recursive = TRUE, full.names = TRUE)]


# Running parallel process

## Defining configuration to use
options(future.globals.maxSize = 20 * 1e9)
data.table::setDTthreads(8)
plan(multicore, workers = 4)

## Getting month number from terminal
month_i <- commandArgs(TRUE) |> as.integer()

## Running the process
OneMonthData <-
  add_take_current_trip(
    ValidZoneSampleByMonth[month_i, data[[1L]]],
    point_mean_distance = PointMeanDistance,
    parquet_path = ValidZoneSampleByMonth[month_i, source_path],
    future.scheduling = 0.6,
    future.chunk.size = 200
  )

# Saving results -----

FileToSave <- paste0("output/take_trip_fst/OneMonthData", month_i, ".fst")
fst::write_fst(OneMonthData, FileToSave)

print(FileToSave)

