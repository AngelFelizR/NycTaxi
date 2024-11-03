### Loading packages to use

## Custom functions
library('project.nyc.taxi')

## To transform data that fits in RAM
library(lubridate)
library(data.table)
library(future)


# Importing data

PointMeanDistance <- fst::read_fst(here("output/cache-data/PointMeanDistance.fst"), as.data.table = TRUE)
ValidZoneSample <- fst::read_fst(here("output/cache-data/ValidZoneSample.fst"), as.data.table = TRUE)


### Split the data by month
ValidZoneSampleByMonth <-
  ValidZoneSample[seq_len(2000)][, request_datetime_extra := request_datetime + minutes(15)
  ][floor_date(request_datetime_extra, unit = "month") == floor_date(request_datetime, unit = "month"),
    .(data = list(.SD)), 
    keyby = c("year", "month"),
    .SDcols = !c("request_datetime_extra")
  ][, source_path := dir(here::here("raw-data/trip-data"), recursive = TRUE, full.names = TRUE)[1]]


# Defining configurations to validate

config_to_test <- 
  CJ(future.chunk.size = c(25, 50, 100, 200),
     future.scheduling = c(0.4, 0.5, 0.6))

config_to_test[,text := paste0("scheduling: ",future.scheduling, " chunk.size:", future.chunk.size)]


# Running the process

options(future.globals.maxSize = 17 * 1e9)


# Configuration 1

setDTthreads(1)
plan(multicore, workers = 7)

for(i in seq_len(nrow(config_to_test))){
  
  tictoc::tic(config_to_test$text[i])
  
  add_take_current_trip(trip_sample = ValidZoneSampleByMonth[1L, data[[1L]]],
                        point_mean_distance = PointMeanDistance,
                        parquet_path = ValidZoneSampleByMonth$source_path,
                        future.scheduling = config_to_test$future.scheduling[i],
                        future.chunk.size = config_to_test$future.chunk.size[i])
  
  tictoc::toc()
  
}


# Configuration 2

setDTthreads(8)
plan(multicore, workers = 4)

for(i in seq_len(nrow(config_to_test))){
  
  tictoc::tic(config_to_test$text[i])
  
  add_take_current_trip(trip_sample = ValidZoneSampleByMonth[1L, data[[1L]]],
                        point_mean_distance = PointMeanDistance,
                        parquet_path = ValidZoneSampleByMonth$source_path,
                        future.scheduling = config_to_test$future.scheduling[i],
                        future.chunk.size = config_to_test$future.chunk.size[i])
  
  tictoc::toc()
  
}

