# Hourly weather -> daily values, the humid-hours (wet period) variable, gap filling
# and the Open-Meteo archive bridge.
# No network: Open-Meteo parsing is tested on a synthetic payload.

hourly_day <- function(date, temp, rh, dpt = 10, rain = 0, srad = 0) {
  n <- length(temp)
  data.frame(date = as.Date(date), temp = temp, rh = rh,
             dpt = rep_len(dpt, n), rain = rep_len(rain, n),
             srad = rep_len(srad, n))
}

test_that("a complete day is summarised: extremes, sums and humid hours", {
  # 24 hours; RH is 90 (counts: 'equal or higher') for 10 hours, 89.9 for 2.
  rh <- c(rep(95, 4), rep(90, 6), rep(89.9, 2), rep(60, 12))
  temp <- seq(10, 33, length.out = 24)
  h <- hourly_day("2026-10-01", temp, rh, dpt = c(rep(8, 12), rep(12, 12)),
                  rain = c(rep(0.5, 4), rep(0, 20)), srad = rep(100, 24))
  d <- aggregate_hourly_weather(h, rh_threshold = 90)
  expect_equal(nrow(d), 1)
  expect_equal(d$tmax, 33)
  expect_equal(d$tmin, 10)
  expect_equal(d$dpt, 10)
  expect_equal(d$rain, 2)
  expect_equal(d$srad, 24 * 100 * 3600 / 1e6)   # 8.64 MJ m-2
  expect_equal(d$rh_wet_hours, 10)
})

test_that("the humidity threshold is configurable", {
  h <- hourly_day("2026-10-01", rep(15, 24), c(rep(95, 5), rep(85, 5), rep(50, 14)))
  expect_equal(aggregate_hourly_weather(h, 90)$rh_wet_hours, 5)
  expect_equal(aggregate_hourly_weather(h, 80)$rh_wet_hours, 10)
})

test_that("incomplete days are NA and missing hours scale the humid-hour count", {
  full <- hourly_day("2026-10-01", rep(15, 24), rep(50, 24))
  partial <- hourly_day("2026-10-02", rep(15, 12), rep(95, 12))      # today, 12 h
  # 22 valid RH hours, 11 of them wet -> 11 * 24 / 22 = 12
  gap <- hourly_day("2026-10-03", rep(15, 24), c(rep(95, 11), rep(50, 11), NA, NA))
  d <- aggregate_hourly_weather(rbind(full, partial, gap), 90)
  expect_equal(d$date, as.Date(c("2026-10-01", "2026-10-02", "2026-10-03")))
  expect_true(all(is.na(d[2, c("tmax", "tmin", "dpt", "rain", "srad", "rh_wet_hours")])))
  expect_equal(d$rh_wet_hours[3], 12)
  expect_equal(d$tmax[3], 15)   # temperature had all 24 hours
})

test_that("the daily result has the columns the weather pipeline expects", {
  d <- aggregate_hourly_weather(hourly_day("2026-10-01", rep(15, 24), rep(50, 24)))
  expect_identical(names(d), c("date", "tmax", "tmin", "dpt", "rain", "srad",
                               "rh_wet_hours"))
  expect_equal(nrow(aggregate_hourly_weather(hourly_day(character(), numeric(), numeric()))), 0)
})

test_that("Open-Meteo hourly JSON is parsed and summarised by local date", {
  hours <- sprintf("2026-10-%02dT%02d:00", rep(1:2, each = 24), rep(0:23, 2))
  payload <- list(hourly = list(
    time = hours,
    temperature_2m = c(rep(12, 24), rep(NA, 24)),
    relative_humidity_2m = c(rep(92, 8), rep(60, 16), rep(NA, 24)),
    dew_point_2m = rep(10, 48),
    precipitation = rep(0.1, 48),
    shortwave_radiation = rep(50, 48)
  ))
  d <- parse_open_meteo_hourly(payload, "Open-Meteo", 90)
  expect_equal(d$date, as.Date(c("2026-10-01", "2026-10-02")))
  expect_equal(d$rh_wet_hours[1], 8)
  expect_true(is.na(d$rh_wet_hours[2]))      # null humidity -> incomplete day
  expect_equal(d$rain[1], 2.4)
  expect_error(parse_open_meteo_hourly(list(hourly = list(time = hours)), "x", 90),
               "missing required hourly")
  expect_error(parse_open_meteo_hourly(list(error = TRUE, reason = "bad"), "x", 90),
               "bad")
})

test_that("preprocessing gap-fills and bounds rh_wet_hours like the other variables", {
  n <- 8
  w <- data.frame(
    date = as.Date("2026-10-01") + 0:(n - 1), rain = 0, tmax = 25, tmin = 15,
    dpt = 12, srad = 15, rh_wet_hours = c(6, NA, NA, 12, 30, 8, 8, 8)
  )
  out <- suppressWarnings(preprocess_weather_data(w, w$date[1], w$date[n]))
  expect_equal(out$rh_wet_hours[1:4], c(6, 8, 10, 12))
  expect_equal(out$rh_wet_hours[5], 24)               # bounded at 24 h
  expect_true(out$imputed_rh_wet_hours[2])
  expect_match(out$imputed_variables[2], "rh_wet_hours")
})

test_that("the archive bridge runs to the first complete Open-Meteo day, not its first row", {
  # Open-Meteo's recent endpoint can return rows of nulls for the oldest days.
  dates <- as.Date("2026-07-16") + 0:40
  meteo <- data.frame(date = dates, tmax = c(rep(NA, 32), rep(20, 9)), tmin = 10,
                      dpt = 8, rain = 0, srad = 15)
  meteo[1:32, c("tmin", "dpt", "rain", "srad")] <- NA
  b <- find_weather_bridge(as.Date("2026-07-30"), meteo)
  expect_equal(b$first_usable, as.Date("2026-08-17"))
  expect_equal(b$archive_start, as.Date("2026-07-31"))
  expect_equal(b$archive_end, as.Date("2026-08-16"))

  # NASA complete up to the day before Open-Meteo is complete: no bridge needed
  ok <- find_weather_bridge(as.Date("2026-08-16"), meteo)
  expect_true(ok$archive_start > ok$archive_end)         # empty window, no bridge
  expect_equal(ok$archive_end, as.Date("2026-08-16"))

  none <- find_weather_bridge(as.Date("2026-09-30"), meteo[1:32, ])
  expect_true(is.na(none$first_usable))
})
