# cd home/rstudio/NycTaxi
# R -e "source('R/01-fine-tune-future-process.R')"

### Loading packages to use

## Custom functions
library('project.nyc.taxi')

## To transform data larger than RAM
library(DBI)
library(duckdb)

## To transform data that fits in RAM
library(lubridate)
library(data.table)
library(future)
library(future.apply)

### Creating DB connections
con <- dbConnect(duckdb(), dbdir = "my-db.duckdb")

### Getting data
PointMeanDistance <- dbGetQuery(con, "SELECT * FROM PointMeanDistance")
ValidZoneSample <- dbGetQuery(con, "SELECT * FROM ValidZoneSample")

### Closing DB connections
dbDisconnect(con, shutdown = TRUE)

### Updating to data.table
setDT(PointMeanDistance)
setDT(ValidZoneSample)

### Split the data by month
ValidZoneSampleByMonth <-
  ValidZoneSample[seq_len(2000)][, request_datetime_extra := request_datetime + minutes(15)
  ][floor_date(request_datetime_extra, unit = "month") == floor_date(request_datetime, unit = "month"),
    .(data = list(.SD)), 
    keyby = c("year", "month"),
    .SDcols = !c("request_datetime_extra")
  ][, source_path := dir("raw-data/trip-data", recursive = TRUE, full.names = TRUE)[1]]


# Defining configurations to validate

config_to_test <- 
  CJ(future.chunk.size = c(25, 50, 100, 200),
     future.scheduling = c(0.4, 0.5, 0.6))

config_to_test[,`:=`(text = paste0("scheduling: ",future.scheduling, " chunk.size:", future.chunk.size),
                     test_result = list())]


# Renning the process

data.table::setDTthreads(1)
options(future.globals.maxSize = 17 * 1e9)
plan(multicore, workers = 7)

for(i in nrow(config_to_test)){
#for(i in seq_len(nrow(config_to_test))){
  
  tictoc::tic(config_to_test$text[i])
  
  ValidZoneSampleByMonth[, add_take_current_trip(trip_sample = data[[1L]],
                                                 point_mean_distance = PointMeanDistance,
                                                 parquet_path = source_path,
                                                 future.scheduling = config_to_test$future.scheduling[i],
                                                 future.chunk.size = config_to_test$future.chunk.size[i]),
                         by = c("year", "month")]
  
  config_to_test$test_result[[i]] <- tictoc::toc()

}

# Saving results

df <- data.frame(
  Scheduling = c(0.4, 0.5, 0.6, 0.4, 0.5, 0.6, 0.4, 0.5, 0.6, 0.4, 0.5),
  Chunk.size = c(25, 25, 25, 50, 50, 50, 100, 100, 100, 200, 200),
  Duration = c(528.970, 428.321, 407.558, 382.662, 384.741, 380.178, 379.015, 379.000, 377.592, 373.415, 379.463)
)

ggplot(df, aes(Chunk.size, Duration, group = Scheduling, color = as.character(Scheduling))) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  labs(title = "Future Performace",
       color = "Scheduling")
