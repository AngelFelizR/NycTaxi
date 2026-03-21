# create-minimal-deterministic-db.R
library(data.table)
library(DBI)
library(lubridate)
library(here)

# ------------------------------------------------------------------
# 1. Tiny test database (only zones 1 ↔ 2)
# ------------------------------------------------------------------
con <- dbConnect(
  duckdb::duckdb(),
  here("tests/testthat/fixtures/minimal-deterministic-db.duckdb")
)

# ------------------------------------------------------------------
# 2. PointMeanDistance (current_position = 2 → possible PU = 1)
# ------------------------------------------------------------------
PointMeanDistance <- data.table(
  PULocationID = 2L,
  DOLocationID = 1L,
  trip_miles_mean = 1.0
)
dbWriteTable(con, "PointMeanDistance", PointMeanDistance, overwrite = TRUE)

# ------------------------------------------------------------------
# 3. Starting trip (driver finishes it at 08:00:00 → simulation starts here)
# ------------------------------------------------------------------
StartTime <- as.POSIXct("2023-01-12 08:00:00", tz = "UTC")

SimulationStartTrips <- data.table(
  trip_id = 999999L,
  hvfhs_license_num = "HV0005",
  wav_match_flag = "N",
  PULocationID = 1L,
  DOLocationID = 2L, # Current_Zone_0 = 2
  request_datetime = StartTime - seconds(600),
  dropoff_datetime = StartTime,
  trip_time = 600,
  driver_pay = 15,
  tips = 2
)

saveRDS(
  SimulationStartTrips,
  here("tests/testthat/fixtures/SimulationStartTrips_minimal.rds")
)

# ------------------------------------------------------------------
# 4. Candidate trips – one per search window (deterministic order guaranteed)
#    All from PU=1 (the only zone the join allows when current=2), DOL=2
# ------------------------------------------------------------------
candidates <- data.table(
  trip_id = 101:104,
  hvfhs_license_num = "HV0005",
  wav_match_flag = "N",
  PULocationID = 1L,
  DOLocationID = 2L,
  request_datetime = StartTime +
    seconds(c(
      10, # first window [08:00, 08:01] → picked
      20 + 300, # after first dropoff (08:05:10) + 10s
      20 + 300 * 2, # after second dropoff
      20 + 300 * 3 # after third dropoff
    )),
  trip_time = 300L, # 5 min
  driver_pay = round(runif(4, 12, 25), 2),
  tips = round(runif(4, 0, 5), 2)
)
candidates[, dropoff_datetime := request_datetime + seconds(trip_time)]

NycTrips <- candidates

dbWriteTable(con, "NycTrips", NycTrips, overwrite = TRUE)

dbDisconnect(con, shutdown = TRUE)
