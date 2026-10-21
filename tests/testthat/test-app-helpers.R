# Dashboard helpers: parsing, validation, settings -> YAML -> loader round trip,
# and an offline run through run_model(). Needs yaml (run_model also needs deSolve).

test_that("parse_numeric_vector accepts commas, semicolons and spaces", {
  expect_equal(parse_numeric_vector("0.35, 0.33; 0.31 0.27", 4), c(0.35, 0.33, 0.31, 0.27))
  expect_error(parse_numeric_vector("0.3, abc", 2), "numbers")
  expect_error(parse_numeric_vector("0.3, 0.2", 4), "exactly 4")
  expect_error(parse_numeric_vector("", 4), "numbers")
})

test_that("parse_overrides reads name: value lines", {
  skip_if_not_installed("yaml")
  expect_null(parse_overrides(""))
  expect_equal(parse_overrides("leaf_infection_rate: 0.3\nfoo: 2"),
               list(leaf_infection_rate = 0.3, foo = 2))
  expect_error(parse_overrides("just text"), "name: value")
})

test_that("a valid input set builds settings", {
  skip_if_not_installed("yaml")
  s <- build_settings(valid_inputs())
  expect_equal(s$weather_mode, "nasa")
  expect_true(s$yaml$weather$nasa_power_only)
  expect_false(s$yaml$weather$nasa_power_open_meteo)
  expect_equal(s$yaml$pathogen$infection_min_wet_hours, 8)
})

test_that("invalid inputs are rejected with readable messages", {
  skip_if_not_installed("yaml")
  expect_error(build_settings(valid_inputs(latitude = 95)), "Latitude")
  expect_error(build_settings(valid_inputs(sowing_date = "nonsense")), "Sowing date")
  expect_error(build_settings(valid_inputs(TT_Anthesis = 900)), "must increase")
  expect_error(build_settings(valid_inputs(spike_peak = 1000)), "start < peak < end")
  expect_error(build_settings(valid_inputs(kernel_start = 2300)), "Kernel formation")
  expect_error(build_settings(valid_inputs(alt_area = 20)), "cannot exceed")
  expect_error(build_settings(valid_inputs(soil_WP = "0.4,0.4,0.4,0.4")), "WP < FC")
  expect_error(build_settings(valid_inputs(soil_FC = "0.3,0.3")), "exactly 4")
  expect_error(build_settings(valid_inputs(rh_threshold = 150)), "humidity threshold")
  expect_error(build_settings(valid_inputs(min_wet_hours = 30)), "Minimum wet period")
  expect_error(build_settings(valid_inputs(min_wet_hours = -1)), "Minimum wet period")
  expect_error(build_settings(valid_inputs(inf_tmin = 30, inf_topt = 25, inf_tmax = 32)),
               "minimum < optimum < maximum")
  expect_error(build_settings(valid_inputs(weather_mode = "csv")), "Upload")
  expect_error(build_settings(valid_inputs(weather_mode = "blended", hindcast = TRUE,
                                           hindcast_date = Sys.Date() - 1)), "Hindcast")
  expect_error(build_settings(valid_inputs(hindcast = TRUE,
                                           hindcast_date = as.Date("2026-01-01"))),
               "on or after the sowing date")
  expect_error(build_settings(valid_inputs(hindcast = TRUE,
                                           hindcast_date = Sys.Date() + 5)), "future")
})

test_that("settings survive the YAML round trip through the model loader", {
  skip_if_not_installed("yaml")
  s <- build_settings(valid_inputs(
    TT_Anthesis = 1250, susceptibility = 0.5,
    overrides = "leaf_infection_rate: 0.3"
  ))
  path <- tempfile(fileext = ".yaml")
  writeLines(settings_to_yaml(s), path)
  loaded <- load_yaml_configuration(path)
  expect_equal(loaded$params$TT_Anthesis, 1250)
  expect_equal(loaded$params$blast_spike_susceptibility,
               model_params$blast_spike_susceptibility * 0.5)
  expect_equal(loaded$params$leaf_infection_rate, 0.3)  # override wins
  expect_equal(loaded$config$weather_source, "NASA_POWER")
  expect_equal(loaded$cultivar_susceptibility, 0.5)
})

test_that("infection and humidity settings round-trip through the loader", {
  skip_if_not_installed("yaml")
  s <- build_settings(valid_inputs(rh_threshold = 85, min_wet_hours = 10,
                                   inf_tmin = 12, inf_topt = 25, inf_tmax = 32))
  path <- tempfile(fileext = ".yaml")
  writeLines(settings_to_yaml(s), path)
  loaded <- suppressWarnings(load_yaml_configuration(path))
  expect_equal(loaded$config$rh_wet_threshold_pct, 85)
  expect_equal(loaded$params$infection_min_wet_hours, 10)
  expect_equal(c(loaded$params$leaf_Tmin, loaded$params$leaf_Topt,
                 loaded$params$leaf_Tmax), c(12, 25, 32))
  # the exported YAML contains no obsolete keys, so the loader stays quiet
  expect_no_warning(load_yaml_configuration(path))
})

test_that("the default settings match the model defaults", {
  skip_if_not_installed("yaml")
  s <- build_settings(valid_inputs(min_wet_hours = NULL, rh_threshold = NULL,
                                   inf_tmin = NULL, inf_topt = NULL, inf_tmax = NULL))
  expect_equal(s$yaml$pathogen$infection_min_wet_hours, 8)
  expect_equal(s$yaml$weather$rh_wet_threshold_pct, 90)
})

test_that("run_model runs offline on a CSV and never throws", {
  skip_if_not_installed("yaml")
  skip_if_not_installed("deSolve")
  suppressPackageStartupMessages(library(deSolve))
  for (module in c("05_soil_water.R", "06_disease_ode.R", "07_phenology.R",
                   "08_plots.R", "10_simulation.R")) source_module(module)

  end_day <- as.Date("2026-10-10")   # the dashboard refuses end dates in the future
  skip_if(Sys.Date() <= end_day)
  dates <- seq(as.Date("2026-04-21"), end_day, by = "day")
  i <- seq_along(dates)
  tmean <- 13 + 0.05 * i
  csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(date = format(dates), rain = ifelse(i %% 4 == 0, 8, 0),
                       tmax = tmean + 5, tmin = tmean - 5, dpt = tmean - 6,
                       srad = 10 + 0.04 * i, rh_wet_hours = 11), csv, row.names = FALSE)

  s <- build_settings(valid_inputs(
    weather_mode = "csv", csv_path = csv,
    hindcast = TRUE, hindcast_date = end_day
  ))
  out <- run_model(s)
  expect_null(out$error)
  expect_gt(nrow(out$result$output), 100)
  expect_true(all(out$result$output$Weather_Source == "CSV"))
  expect_equal(out$result$wheat_blast_summary$Days_Wet_Period_Met,
               nrow(out$result$weather))     # 11 h/day >= the default 8 h
  expect_true(any(grepl("Infection rule", out$log)))
})
