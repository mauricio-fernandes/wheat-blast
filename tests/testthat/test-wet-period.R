# The infection rule: a favorable temperature AND a minimum wet period.

test_that("the wet-period gate is 1 at or above the minimum hours, else 0", {
  expect_equal(calculate_wet_period_gate(c(0, 7.9, 8, 8.1, 24)), c(0, 0, 1, 1, 1))
  expect_equal(calculate_wet_period_gate(c(5, 6), min_hours = 6), c(0, 1))
  expect_equal(calculate_wet_period_gate(c(0, 0), min_hours = 0), c(1, 1))
})

test_that("unknown hours give NA, not a silent zero", {
  expect_true(is.na(calculate_wet_period_gate(NA_real_)))
  expect_equal(calculate_wet_period_gate(c(10, NA)), c(1, NA))
})

test_that("an invalid minimum number of hours is rejected", {
  expect_error(calculate_wet_period_gate(10, min_hours = -1), "between 0 and 24")
  expect_error(calculate_wet_period_gate(10, min_hours = 30), "between 0 and 24")
})

test_that("the infection temperature response is a cardinal function", {
  f <- function(t) vapply(t, cardinal_temperature_response, numeric(1),
                          tmin = 15, topt = 28, tmax = 30)
  expect_equal(f(c(10, 15, 21.5, 28, 29, 30, 35)), c(0, 0, 0.5, 1, 0.5, 0, 0))
  expect_equal(f(NA_real_), 0)
})

test_that("both conditions are needed: favorable only if temperature AND wet period", {
  temp <- vapply(c(12, 22, 22, 29.9), cardinal_temperature_response, numeric(1),
                 tmin = 15, topt = 28, tmax = 30)
  gate <- calculate_wet_period_gate(c(12, 12, 4, 12), 8)
  favorability <- temp * gate
  expect_equal(favorability[1], 0)          # wet but too cold
  expect_gt(favorability[2], 0)             # warm enough and wet
  expect_equal(favorability[3], 0)          # warm but not wet long enough
  expect_gt(favorability[4], 0)
  expect_lt(favorability[4], favorability[2])
})

test_that("the removed dew / canopy-moisture settings are reported when found in a YAML", {
  skip_if_not_installed("yaml")
  path <- tempfile(fileext = ".yaml")
  writeLines(c("weather:", "  nasa_power_only: true", "  hourly: true",
               "pathogen:", "  dew_enabled: true", "  dew_spread_onset_C: 2.0",
               "  spike_moisture_threshold: 0.65"), path)
  expect_warning(load_yaml_configuration(path), "obsolete")
  clean <- tempfile(fileext = ".yaml")
  writeLines(c("weather:", "  nasa_power_only: true"), clean)
  expect_no_warning(load_yaml_configuration(clean))
})
