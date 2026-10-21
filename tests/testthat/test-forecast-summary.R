# summarize_forecast_window() on a 10-day synthetic record:
# days 1-4 NASA_POWER, 5-6 OPEN_METEO_RECENT, 7-10 OPEN_METEO_FORECAST.

run_fixture <- function(...) {
  fx <- make_fixture(...)
  summarize_forecast_window(fx$output, fx$weather, fixture_params)
}

test_that("window dates, phenology and weather totals are summarized", {
  fx <- make_fixture()
  fc <- summarize_forecast_window(fx$output, fx$weather, fixture_params)
  s <- fc$summary
  expect_equal(s$Forecast_Days, 4)
  expect_equal(s$Forecast_Start_Date, fx$weather$date[7])
  expect_equal(s$Forecast_End_Date, fx$weather$date[10])
  expect_equal(s$Phenology_At_Start, "FlagLeaf")               # TT 1100
  expect_equal(s$Phenology_At_End, "InitialKernelFormation")   # TT 1400
  expect_equal(s$Forecast_Rain_Total_mm, 12)
  expect_equal(s$Forecast_Rain_Days_ge_1mm, 2)
  expect_equal(s$Forecast_Mean_Tmean_C, 21)
  expect_equal(s$Forecast_Mean_RH_Wet_Hours, 10)      # (5 + 12 + 14 + 9) / 4
  expect_equal(s$Days_Wet_Period_Met, 3)              # >= 8 h on days 8-10
  expect_equal(s$Days_Favorable_Temperature, 4)
  expect_equal(s$Leaf_Severity_Pct_At_End, 1)
})

test_that("heading window, infection conditions and spores are counted", {
  s <- run_fixture()$summary
  expect_equal(s$Days_In_Heading_Window, 4)
  expect_equal(s$Max_Heading_Susceptibility, 0.5)
  expect_equal(s$Days_Infection_Conditions_Met, 3)  # day 7 has only 5 wet hours
  expect_match(s$Signal, "3 of 4")
  expect_equal(s$Airborne_Spores_At_Start, 10)
  expect_equal(s$Airborne_Spores_At_End, 30)
  expect_equal(s$Airborne_Spores_Max, 40)
})

test_that("kernel infection gain and peak day are computed from day-start states", {
  fx <- make_fixture()
  s <- summarize_forecast_window(fx$output, fx$weather, fixture_params)$summary
  expect_equal(s$Kernel_Infection_At_Start, 0)
  expect_equal(s$Kernel_Infection_At_End, 6)
  expect_equal(s$Kernel_Infection_Gain, 6)
  expect_equal(s$Kernel_Infection_Peak_Daily_Gain, 3)
  expect_equal(s$Kernel_Infection_Peak_Gain_Date, fx$weather$date[9])
  expect_equal(s$Pre_Forecast_Max_Daily_Gain, 0)
})

test_that("daily table covers the whole record; the last day's change is NA", {
  fc <- run_fixture()
  expect_equal(nrow(fc$daily), 10)
  expect_equal(nrow(fc$window), 4)
  expect_true(all(fc$window$Source == "OPEN_METEO_FORECAST"))
  expect_true(is.na(fc$daily$Kernel_Infection_Change[10]))
  expect_equal(fc$daily$Kernel_Infection_Change[7:9], c(1, 2, 3))
  expect_equal(fc$daily$Infection_Conditions_Met,
               c(rep(FALSE, 7), TRUE, TRUE, TRUE))
})

test_that("the signal distinguishes the three situations", {
  outside <- run_fixture(heading = rep(0, 10))$summary
  expect_match(outside$Signal, "outside the heading window")
  expect_equal(outside$Days_In_Heading_Window, 0)

  dry <- run_fixture(wet_hours = rep(2, 10))$summary
  expect_match(dry$Signal, "no forecast day combines")
  expect_equal(dry$Days_Infection_Conditions_Met, 0)

  cold <- run_fixture(temp_factor = rep(0, 10))$summary
  expect_match(cold$Signal, "no forecast day combines")
  expect_equal(cold$Days_Favorable_Temperature, 0)

  wet <- run_fixture()$summary
  expect_match(wet$Signal, "infection conditions met")
})

test_that("no gain gives NA peak date and a zero peak gain", {
  s <- run_fixture(infection = rep(0, 10))$summary
  expect_true(is.na(s$Kernel_Infection_Peak_Gain_Date))
  expect_equal(s$Kernel_Infection_Peak_Daily_Gain, 0)
  expect_equal(s$Kernel_Infection_Gain, 0)
})

test_that("a window that starts on day 1 has no pre-forecast reference gain", {
  s <- run_fixture(source = rep("OPEN_METEO_FORECAST", 10))$summary
  expect_equal(s$Forecast_Days, 10)
  expect_true(is.na(s$Pre_Forecast_Max_Daily_Gain))
})

test_that("NULL is returned when there is nothing to summarize", {
  expect_null(run_fixture(source = rep("NASA_POWER", 10)))
  fx <- make_fixture()
  fx$weather$weather_source <- NULL
  expect_null(summarize_forecast_window(fx$output, fx$weather, fixture_params))
})

test_that("printing works with and without a forecast", {
  expect_message(print_forecast_summary(NULL), "no Open-Meteo forecast days")
  expect_output(print_forecast_summary(run_fixture()),
                "Forecast-window summary")
})

test_that("phenology_stage_at maps thermal time to the stage reached", {
  expect_equal(phenology_stage_at(0, fixture_params), "Pre-emergence")
  expect_equal(phenology_stage_at(1300, fixture_params), "Anthesis")
  expect_equal(phenology_stage_at(2400, fixture_params), "PhysiologicalMaturity")
  expect_true(is.na(phenology_stage_at(NA_real_, fixture_params)))
})

test_that("infection needs a favorable temperature AND the minimum wet period", {
  # day 8 has 12 wet hours but an unfavorable temperature; day 7 is favorable but dry
  fx <- make_fixture(temp_factor = c(rep(1, 7), 0, 0.5, 1))
  fc <- summarize_forecast_window(fx$output, fx$weather, fixture_params)
  expect_equal(fc$daily$Infection_Conditions_Met,
               c(rep(FALSE, 8), TRUE, TRUE))
  expect_equal(fc$summary$Days_Infection_Conditions_Met, 2)
  expect_equal(fc$summary$Days_Wet_Period_Met, 3)
  expect_equal(fc$summary$Days_Favorable_Temperature, 3)

  # a threshold of exactly the observed hours counts (equal or higher)
  strict <- fixture_params
  strict$infection_min_wet_hours <- 12
  s <- summarize_forecast_window(fx$output, fx$weather, strict)$summary
  expect_equal(s$Days_Wet_Period_Met, 2)   # days 8 (12 h) and 9 (14 h)
})
