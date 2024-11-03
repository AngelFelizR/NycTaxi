
## Custom functions
library('project.nyc.taxi')

## To manage relative paths
library(here)

## To transform data that fits in RAM
library(data.table)
library(lubridate)
library(future)


# Importing data

PointMeanDistance <- fst::read_fst(here("output/cache-data/PointMeanDistance.fst"), as.data.table = TRUE)
ValidZoneSample <- fst::read_fst(here("output/cache-data/ValidZoneSample.fst"), as.data.table = TRUE)


# Split the data by month and link with the original parquet files

ValidZoneSampleByMonth <-
  ValidZoneSample[
    j = request_datetime_extra := request_datetime + minutes(15)
  ][floor_date(request_datetime_extra, unit = "month") == floor_date(request_datetime, unit = "month"),
    .(data = list(.SD)), 
    keyby = c("year", "month"),
    .SDcols = !c("request_datetime_extra")
  ][, source_path := dir(here("raw-data/trip-data"), recursive = TRUE, full.names = TRUE)]


# Running parallel process

data.table::setDTthreads(8)
options(future.globals.maxSize = 20 * 1e9)
plan(multicore, workers = 4)


# Getting params from command line arguments
month_i <- commandArgs(TRUE) |> as.integer()
  
OneMonthData <-
  add_take_current_trip(
    ValidZoneSampleByMonth[month_i, data[[1L]]],
    point_mean_distance = PointMeanDistance,
    parquet_path = ValidZoneSampleByMonth[month_i, source_path],
    future.scheduling = 0.4,
    future.chunk.size = 200
  )

FileToSave <- paste0("output/take_trip_fst/OneMonthData", month_i, ".fst")

fst::write_fst(OneMonthData, FileToSave)

print(FileToSave)

