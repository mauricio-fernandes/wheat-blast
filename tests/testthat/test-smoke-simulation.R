# Offline end-to-end runs on synthetic CSV weather (no network). They check that
# the modules work together and that the infection rule behaves as specified, not
# that the epidemiology is right. Skipped when deSolve is not installed.

synthetic_weather_csv <- function(wet_hours, dates = seq(as.Date("2026-04-21"),
                                                         as.Date("2026-10-31"), by = "day"),
                                  warm = 0) {
  i <- seq_along(dates)
  tmean <- 13 + 0.05 * i + warm
  csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    date = format(dates), rain = ifelse(i %% 4 == 0, 8, 0),
    tmax = tmean + 5, tmin = tmean - 5, dpt = tmean - 6,
    srad = 10 + 0.04 * i, rh_wet_hours = rep_len(wet_hours, length(i))
  ), csv, row.names = FALSE)
  csv
}

run_synthetic <- function(wet_hours, min_wet_hours = 8, warm = 0) {
  csv <- synthetic_weather_csv(wet_hours, warm = warm)
  cfg <- simulation_config
  cfg$start_date <- as.Date("2026-06-20")   # model starts 60 days earlier
  cfg$weather_source <- "CSV"
  cfg$weather_file_path <- csv
  cfg$end_date <- as.Date("2026-10-31")
  cfg$make_plots <- FALSE
  params <- model_params
  params$infection_min_wet_hours <- min_wet_hours
  suppressMessages(suppressWarnings(
    run_simulation(config = cfg, params = params, soil_layers = soil_layers_df)
  ))
}

load_simulation_modules <- function() {
  skip_if_not_installed("deSolve")
  suppressPackageStartupMessages(library(deSolve))
  for (module in c("05_soil_water.R", "06_disease_ode.R", "07_phenology.R",
                   "08_plots.R", "10_simulation.R")) {
    source_module(module)
  }
}

test_that("a synthetic-weather run completes and the outputs are sane", {
  load_simulation_modules()
  res <- run_synthetic(wet_hours = 12)

  expect_gt(nrow(res$output), 100)
  expect_false(anyNA(res$output$Blast_Kernel_Infection))
  expect_true(all(res$output$Blast_Kernel_Infection >= 0))
  expect_false(is.unsorted(res$output$Thermal_Time))
  expect_true(all(res$output$Weather_Source == "CSV"))
  expect_null(res$forecast_summary)  # no forecast days in CSV mode
  expect_equal(res$wheat_blast_summary$Forecast_Days_Simulated, 0)
  expect_false(any(c("Canopy_Moisture_Index", "Dew_Favorability") %in% names(res$output)))
  expect_true(all(c("RH_Wet_Hours", "Wet_Period_Met", "Infection_Temp_Factor",
                    "Infection_Favorability") %in% names(res$output)))
  expect_equal(unique(res$output$RH_Wet_Hours), 12)
  expect_true(all(res$output$Wet_Period_Met == 1))
})

test_that("without the minimum wet period there is no infection", {
  load_simulation_modules()
  dry <- run_synthetic(wet_hours = 4)      # below the 8 h minimum every day
  expect_true(all(dry$output$Wet_Period_Met == 0))
  expect_equal(max(dry$output$Blast_Kernel_Infection), 0)
  expect_equal(max(dry$output$Leaf_Latent), 0)
  expect_equal(dry$wheat_blast_summary$Days_Wet_Period_Met, 0)

  wet <- run_synthetic(wet_hours = 12)
  expect_gt(max(wet$output$Blast_Kernel_Infection), 0)
  expect_gt(max(wet$output$Leaf_Disease_Severity_Pct), 0)
})

test_that("the minimum number of wet hours is configurable and inclusive", {
  load_simulation_modules()
  at_limit <- run_synthetic(wet_hours = 10, min_wet_hours = 10)   # equal counts
  expect_true(all(at_limit$output$Wet_Period_Met == 1))
  expect_gt(max(at_limit$output$Blast_Kernel_Infection), 0)

  above <- run_synthetic(wet_hours = 10, min_wet_hours = 10.5)
  expect_true(all(above$output$Wet_Period_Met == 0))
  expect_equal(max(above$output$Blast_Kernel_Infection), 0)
})

test_that("a wet period alone is not enough: temperature outside the range blocks infection", {
  load_simulation_modules()
  hot <- run_synthetic(wet_hours = 12, warm = 25)   # daily mean above the maximum
  expect_true(all(hot$output$Wet_Period_Met == 1))
  expect_true(all(hot$output$Infection_Temp_Factor == 0))
  expect_equal(max(hot$output$Blast_Kernel_Infection), 0)
})
