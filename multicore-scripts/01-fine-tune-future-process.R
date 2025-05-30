
# Loading Packages ----

## To transform data that fits in RAM
library(lubridate)
library(data.table)
library(future)

## To import and export data frames as binary files
library(fst)

## To manage relative paths
library(here)

## Custom functions
source(here("R"))


# Importing data ----

PointMeanDistance <-
  here("output/cache-data/PointMeanDistance.fst") |>
  read_fst(as.data.table = TRUE)

ValidZoneSample <-
  here("output/cache-data/ValidZoneSample.fst") |>
  read_fst(as.data.table = TRUE)


# Selecting the data to use -----

ValidZoneSampleByMonth <-
  
  ## we only need 2k sample for each test
  ValidZoneSample[seq_len(2000L)
  
  ## Excluding trips that took place in the last 15 min minutes
  ## to be able to run this process month by month as solution
  ## to avoid running the process on disk which can be really slow
  ][, request_datetime_extra := request_datetime + minutes(15)
  ][floor_date(request_datetime_extra, unit = "month") == floor_date(request_datetime, unit = "month"),
    .(data = list(.SD)), 
    keyby = c("year", "month"),
    .SDcols = !c("request_datetime_extra")
    
  ## Adding path first parquet month
  ][, source_path := here("raw-data/trip-data/year=2022/month=01/part-0.parquet")]


# Defining configurations to validate ----

config_to_test <- 
  CJ(future.chunk.size = c(25, 50, 100, 200),
     future.scheduling = c(0.4, 0.5, 0.6))

config_to_test[,text := paste0("scheduling: ",future.scheduling, " chunk.size:", future.chunk.size)]


# Running the process ----

## Making sure to able to use all the RAM available
options(future.globals.maxSize = 17 * 1e9)


## Configuration 1 ----

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


## Configuration 2 -----

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

