
### Loading packages to use

## Custom functions
library('project.nyc.taxi')

## To manage relative paths
library(here)

## To transform data larger than RAM
library(DBI)
library(duckdb)

## To transform data that fits in RAM
library(data.table)
library(lubridate)
library(future)
library(future.apply)

## To create plots
library(ggplot2)
library(scales)


### Creating DB connections
con <- dbConnect(duckdb(), dbdir = here("my-db.duckdb"))

### Getting data
PointMeanDistance <- dbGetQuery(con, "SELECT * FROM PointMeanDistance")
ValidZoneSample <- dbGetQuery(con, "SELECT * FROM ValidZoneSample")

dbDisconnect(con, shutdown = TRUE)


### Updating to data.table
setDT(PointMeanDistance)
setDT(ValidZoneSample)

### Split the data by month
ValidZoneSampleByMonth <-
  ValidZoneSample[1:60][, request_datetime_extra := request_datetime + minutes(15)
  ][floor_date(request_datetime_extra, unit = "month") == floor_date(request_datetime, unit = "month"),
    .(data = list(.SD)), 
    keyby = c("year", "month"),
    .SDcols = !c("request_datetime_extra")
  ][, source_path := dir("raw-data/trip-data", recursive = TRUE, full.names = TRUE)[1]]


data.table::setDTthreads(1)
options(future.globals.maxSize = 10 * 1e9)
plan(multicore, workers = 7)

tictoc::tic.clear()

tictoc::tic("N_01")
for(n_i in seq_len(nrow(ValidZoneSampleByMonth))){
  add_take_current_trip(trip_sample = ValidZoneSampleByMonth[n_i]$data[[1]],
                        point_mean_distance = PointMeanDistance,
                        parquet_path = ValidZoneSampleByMonth[n_i]$source_path,
                        future.scheduling = 1)
}
tictoc::toc()


tictoc::tic("N_26")
for(n_i in seq_len(nrow(ValidZoneSampleByMonth))){
  add_take_current_trip(trip_sample = ValidZoneSampleByMonth[n_i]$data[[1]],
                        point_mean_distance = PointMeanDistance,
                        parquet_path = ValidZoneSampleByMonth[n_i]$source_path,
                        future.scheduling = 26)
}
tictoc::toc()
