# impute_short_weather_gaps() and preprocess_weather_data()

dates_of <- function(n) as.Date("2026-05-01") + seq_len(n) - 1

test_that("an internal gap is linearly interpolated and flagged", {
  res <- impute_short_weather_gaps(c(1, NA, NA, 4, 5), dates_of(5), "tmax")
  expect_equal(res$values, c(1, 2, 3, 4, 5))
  expect_equal(res$imputed, c(FALSE, TRUE, TRUE, FALSE, FALSE))
})

test_that("leading and trailing gaps take the nearest observed value", {
  lead <- impute_short_weather_gaps(c(NA, NA, 3, 4), dates_of(4), "tmax")
  expect_equal(lead$values, c(3, 3, 3, 4))
  expect_equal(lead$imputed, c(TRUE, TRUE, FALSE, FALSE))
  trail <- impute_short_weather_gaps(c(1, 2, NA), dates_of(3), "tmax")
  expect_equal(trail$values, c(1, 2, 2))
})

test_that("a gap exactly at the limit is filled, one day longer is an error", {
  ok <- impute_short_weather_gaps(c(1, NA, NA, NA, 5), dates_of(5), "tmax")
  expect_equal(ok$values, c(1, 2, 3, 4, 5))
  expect_error(
    impute_short_weather_gaps(c(1, NA, NA, NA, NA, 6), dates_of(6), "tmax"),
    "exceeds"
  )
  expect_error(
    impute_short_weather_gaps(c(1, NA, NA, 4), dates_of(4), "tmax",
                              max_gap_days = 1L),
    "exceeds"
  )
})

test_that("NASA fill values (<= -900) count as missing", {
  res <- impute_short_weather_gaps(c(1, -999, 3), dates_of(3), "rain")
  expect_equal(res$values, c(1, 2, 3))
  expect_equal(res$imputed, c(FALSE, TRUE, FALSE))
})

test_that("complete data pass through unchanged", {
  res <- impute_short_weather_gaps(c(1, 2, 3), dates_of(3), "srad")
  expect_equal(res$values, c(1, 2, 3))
  expect_false(any(res$imputed))
})

test_that("variables without enough observations are rejected", {
  expect_error(
    impute_short_weather_gaps(rep(NA_real_, 3), dates_of(3), "rain"),
    "no usable observations"
  )
  expect_error(
    impute_short_weather_gaps(c(1, NA, NA), dates_of(3), "rain"),
    "at least two"
  )
})

# --- preprocess_weather_data() -----------------------------------------------

make_weather <- function(n = 7) {
  data.frame(
    date = dates_of(n), rain = 0, tmax = 20, tmin = 10, dpt = 8, srad = 12,
    rh_wet_hours = 9
  )
}
window_start <- as.Date("2026-05-01")

test_that("an omitted calendar day is restored, flagged and warned about", {
  w <- make_weather()[-3, ]
  expect_warning(
    res <- preprocess_weather_data(w, window_start, window_start + 6),
    "Interpolated"
  )
  expect_equal(nrow(res), 7)
  expect_equal(res$date, window_start + 0:6)
  expect_equal(res$weather_imputed,
               c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(res$imputed_variables[3], "tmax,tmin,dpt,rain,srad,rh_wet_hours")
  expect_equal(res$rh_wet_hours[3], 9)
  expect_equal(res$tmax[3], 20)
})

test_that("the daily rh_wet_hours column is required and carried through", {
  res <- preprocess_weather_data(make_weather(), window_start, window_start + 6)
  expect_equal(res$rh_wet_hours, rep(9, 7))
  no_rh <- make_weather()
  no_rh$rh_wet_hours <- NULL
  expect_error(
    preprocess_weather_data(no_rh, window_start, window_start + 6),
    "rh_wet_hours"
  )
})

test_that("long gaps and impossible rows stop the run", {
  long_gap <- make_weather(10)
  long_gap$rain[2:6] <- NA  # five consecutive missing days
  expect_error(
    preprocess_weather_data(long_gap, window_start, window_start + 9),
    "exceeds"
  )
  bad <- make_weather()
  bad$tmax[2] <- 5  # below tmin = 10
  expect_error(
    preprocess_weather_data(bad, window_start, window_start + 6),
    "tmax < tmin"
  )
})
