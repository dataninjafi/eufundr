test_that("get_kohesio_projects() returns a data frame", {
  df <- get_kohesio_projects("FI")
  expect_s3_class(df, "data.frame")
  expect_true("country" %in% names(df))
})

test_that("get_kohesio_beneficiaries() returns a data frame", {
  df <- get_kohesio_beneficiaries("FI")
  expect_s3_class(df, "data.frame")
  expect_true("country" %in% names(df))
})

test_that("get_fts_data() filters by country", {
  df <- get_fts_data(2020, country = "Finland")
  expect_s3_class(df, "data.frame")
  expect_true(all(df$beneficiary_country == "Finland"))
})

test_that("get_horizon_europe() filters by country", {
  df <- get_horizon_europe("FI")
  expect_s3_class(df, "data.frame")
  expect_true("hakijanimi" %in% names(df))
})
