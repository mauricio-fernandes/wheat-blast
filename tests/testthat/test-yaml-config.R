# load_yaml_configuration(): thermal time under crop:, infection settings, weather keys.

test_that("thermal-time parameters under crop: are applied (they were ignored)", {
  skip_if_not_installed("yaml")
  path <- tempfile(fileext = ".yaml")
  writeLines(c(
    "crop:",
    "  sowing_date: 2026-06-20",
    "  cultivar_susceptibility: 1",
    "  TT_Anthesis: 1250",
    "  TT_FlagLeaf: 990",
    "  TT_GrainFill: 2100",
    "  TTM: 2300",
    "weather:",
    "  nasa_power_only: true"
  ), path)
  res <- load_yaml_configuration(path)
  expect_equal(res$params$TT_Anthesis, 1250)
  expect_equal(res$params$TT_FlagLeaf, 990)
  expect_equal(res$params$TT_GrainFill, 2100)
  expect_equal(res$params$TTM, 2300)
  expect_equal(res$params$TT_Emergence, model_params$TT_Emergence)  # untouched
})

test_that("infection settings and the humidity threshold are read and validated", {
  skip_if_not_installed("yaml")
  path <- tempfile(fileext = ".yaml")
  writeLines(c(
    "weather:", "  nasa_power_only: true", "  rh_wet_threshold_pct: 85",
    "pathogen:", "  infection_min_wet_hours: 10",
    "  infection_Tmin_C: 12", "  infection_Topt_C: 25", "  infection_Tmax_C: 32"
  ), path)
  res <- load_yaml_configuration(path)
  expect_equal(res$config$rh_wet_threshold_pct, 85)
  expect_equal(res$params$infection_min_wet_hours, 10)
  expect_equal(c(res$params$leaf_Tmin, res$params$leaf_Topt, res$params$leaf_Tmax),
               c(12, 25, 32))

  defaults <- tempfile(fileext = ".yaml")
  writeLines(c("weather:", "  nasa_power_only: true"), defaults)
  d <- load_yaml_configuration(defaults)
  expect_equal(d$params$infection_min_wet_hours, 8)
  expect_equal(d$config$rh_wet_threshold_pct, 90)

  bad <- tempfile(fileext = ".yaml")
  writeLines(c("weather:", "  nasa_power_only: true",
               "pathogen:", "  infection_min_wet_hours: 30"), bad)
  expect_error(load_yaml_configuration(bad), "between 0 and 24")
  writeLines(c("weather:", "  nasa_power_only: true", "pathogen:",
               "  infection_Tmin_C: 30", "  infection_Topt_C: 25",
               "  infection_Tmax_C: 32"), bad)
  expect_error(load_yaml_configuration(bad), "Tmin_C < infection_Topt_C")
})
