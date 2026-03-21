it("baseline policy (deterministic) produces exactly the expected trip sequence", {
  # ARRANGE
  con <- dbConnect(
    duckdb::duckdb(),
    test_path("fixtures/minimal-deterministic-db.duckdb")
  )
  on.exit(dbDisconnect(con, shutdown = TRUE))

  start <- readRDS(test_path("fixtures/SimulationStartTrips_minimal.rds"))

  # ACT – deterministic baseline (seeds = NULL, no model)
  result <- simulate_trips(con, start)

  # ASSERT
  expect_equal(
    result$sim_trip_id,
    c(101L, 102L, 103L, 104L) # only the searched/accepted trips, in order
  )

  expect_equal(
    result$simulation_id,
    rep(999999L, 4) # every row links back to the starting trip
  )

  # Company stays constant (Powell: Taxi_Company^n fixed for the whole workday)
  expect_equal(unique(result$sim_hvfhs_license_num), "HV0005")

  # Total working time ≤ 8.5 h (Powell: last trip may finish after 8 h)
  total_hours <- result[,
    .(
      hours = difftime(
        max(sim_dropoff_datetime),
        min(sim_request_datetime),
        units = "hours"
      ) |>
        as.double()
    ),
    by = simulation_id
  ]
  expect_lte(total_hours$hours, 8.5)
})
