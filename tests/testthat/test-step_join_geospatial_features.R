# tests/testthat/test-step_join_geospatial_features.R

test_that("direct join without prefix works", {
  # Setup test data
  main_data <- data.frame(
    region_id = c("A", "B", "C"),
    value = c(10, 20, 30)
  )

  spatial_data <- data.frame(
    region_id = c("A", "B", "C"),
    latitude = c(40.7, 34.1, 41.9),
    longitude = c(-74.0, -118.2, -87.6),
    population = c(1000, 2000, 1500)
  )

  recipe <- recipes::recipe(value ~ ., data = main_data) |>
    step_join_geospatial_features(
      region_id,
      spatial_features = spatial_data
    )

  prepped_recipe <- recipes::prep(recipe, training = main_data)
  baked_data <- recipes::bake(prepped_recipe, new_data = main_data)

  # Check that spatial columns were added
  expect_true("latitude" %in% names(baked_data))
  expect_true("longitude" %in% names(baked_data))
  expect_true("population" %in% names(baked_data))

  # Check that original data is preserved
  expect_equal(baked_data$value, main_data$value)
  expect_equal(baked_data$region_id, main_data$region_id)

  # Check that join worked correctly
  expect_equal(baked_data$latitude, c(40.7, 34.1, 41.9))
  expect_equal(baked_data$population, c(1000, 2000, 1500))
})

test_that("join with prefixes works", {
  # Create data with prefixed columns
  main_data_prefix <- data.frame(
    start_region_id = c("A", "B", "C"),
    end_region_id = c("C", "B", "A"),
    value = c(10, 20, 30)
  )

  spatial_data <- data.frame(
    region_id = c("A", "B", "C"),
    latitude = c(40.7, 34.1, 41.9),
    longitude = c(-74.0, -118.2, -87.6),
    population = c(1000, 2000, 1500)
  )

  recipe <- recipes::recipe(value ~ ., data = main_data_prefix) |>
    step_join_geospatial_features(
      ends_with("region_id"),
      spatial_features = spatial_data,
      col_prefix = c("start_", "end_")
    )

  prepped_recipe <- recipes::prep(recipe, training = main_data_prefix)
  baked_data <- recipes::bake(prepped_recipe, new_data = main_data_prefix)

  # Check that prefixed spatial columns were added
  expect_true("start_latitude" %in% names(baked_data))
  expect_true("start_longitude" %in% names(baked_data))
  expect_true("end_latitude" %in% names(baked_data))
  expect_true("end_longitude" %in% names(baked_data))

  # Check that original data is preserved
  expect_equal(baked_data$value, main_data_prefix$value)
  expect_equal(baked_data$start_region_id, main_data_prefix$start_region_id)

  # Check that joins worked correctly
  expect_equal(baked_data$start_latitude, c(40.7, 34.1, 41.9))
  expect_equal(baked_data$end_latitude, c(41.9, 34.1, 40.7))
})

test_that("error when join columns not found in spatial features", {
  main_data <- data.frame(
    region_id = c("A", "B", "C"),
    value = c(10, 20, 30)
  )

  bad_spatial_data <- data.frame(
    wrong_id = c("A", "B", "C"), # Different column name
    latitude = c(40.7, 34.1, 41.9)
  )

  recipe <- recipes::recipe(value ~ ., data = main_data) |>
    step_join_geospatial_features(
      region_id,
      spatial_features = bad_spatial_data
    )

  expect_error(
    recipes::prep(recipe, training = main_data),
    "The defined terms cannot be found in spatial_features"
  )
})

test_that("error with prefixes when columns still don't match", {
  main_data_bad_prefix <- data.frame(
    bad_prefix_region_id = c("A", "B", "C"),
    value = c(10, 20, 30)
  )

  spatial_data <- data.frame(
    region_id = c("A", "B", "C"),
    latitude = c(40.7, 34.1, 41.9)
  )

  recipe <- recipes::recipe(value ~ ., data = main_data_bad_prefix) |>
    step_join_geospatial_features(
      ends_with("region_id"),
      spatial_features = spatial_data,
      col_prefix = c("wrong_prefix_") # Wrong prefix
    )

  expect_error(
    recipes::prep(recipe, training = main_data_bad_prefix),
    "The defined terms cannot be found in spatial_features"
  )
})

test_that("empty spatial features handled gracefully", {
  main_data <- data.frame(
    region_id = c("A", "B", "C"),
    value = c(10, 20, 30)
  )

  spatial_data <- data.frame(
    region_id = c("A", "B", "C"),
    latitude = c(40.7, 34.1, 41.9),
    longitude = c(-74.0, -118.2, -87.6)
  )

  empty_spatial <- spatial_data[0, ]

  recipe <- recipes::recipe(value ~ ., data = main_data) |>
    step_join_geospatial_features(
      region_id,
      spatial_features = empty_spatial
    )

  prepped_recipe <- recipes::prep(recipe, training = main_data)
  baked_data <- recipes::bake(prepped_recipe, new_data = main_data)

  # Should still have original columns
  expect_true("region_id" %in% names(baked_data))
  expect_true("value" %in% names(baked_data))
  # Spatial columns should be present but with NA values
  expect_true("latitude" %in% names(baked_data))
  expect_true(all(is.na(baked_data$latitude)))
})

test_that("missing matches in spatial data handled with NA", {
  main_data_with_missing <- data.frame(
    region_id = c("A", "B", "D"), # "D" not in spatial_data
    value = c(10, 20, 30)
  )

  spatial_data <- data.frame(
    region_id = c("A", "B", "C"),
    latitude = c(40.7, 34.1, 41.9),
    population = c(1000, 2000, 1500)
  )

  recipe <- recipes::recipe(value ~ ., data = main_data_with_missing) |>
    step_join_geospatial_features(
      region_id,
      spatial_features = spatial_data
    )

  prepped_recipe <- recipes::prep(recipe, training = main_data_with_missing)
  baked_data <- recipes::bake(prepped_recipe, new_data = main_data_with_missing)

  # Should have NA for missing region
  expect_true(is.na(baked_data$latitude[baked_data$region_id == "D"]))
  expect_true(is.na(baked_data$population[baked_data$region_id == "D"]))
})

test_that("print method works for both unprepared and prepared recipes", {
  main_data <- data.frame(
    region_id = c("A", "B", "C"),
    value = c(10, 20, 30)
  )

  spatial_data <- data.frame(
    region_id = c("A", "B", "C"),
    latitude = c(40.7, 34.1, 41.9)
  )

  recipe <- recipes::recipe(value ~ ., data = main_data) |>
    step_join_geospatial_features(
      region_id,
      spatial_features = spatial_data
    )

  # Should not error for unprepared recipe
  expect_no_error(print(recipe))

  prepped_recipe <- recipes::prep(recipe, training = main_data)
  # Should not error for prepared recipe
  expect_no_error(print(prepped_recipe))
})

test_that("required packages are correctly specified", {
  main_data <- data.frame(
    region_id = c("A", "B", "C"),
    value = c(10, 20, 30)
  )

  spatial_data <- data.frame(
    region_id = c("A", "B", "C"),
    latitude = c(40.7, 34.1, 41.9)
  )

  recipe <- recipes::recipe(value ~ ., data = main_data) |>
    step_join_geospatial_features(
      region_id,
      spatial_features = spatial_data
    )

  required_pkgs <- required_pkgs(recipe)
  expect_true("data.table" %in% required_pkgs)
})
