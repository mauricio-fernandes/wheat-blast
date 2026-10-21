# Kernel-infection risk classes, report text and (when pandoc is available) a render.

test_that("risk classes follow the 0.05 / 0.1 / 0.2 / 0.3 / 0.4 limits; a limit belongs to the higher class", {
  x <- c(0, 0.049, 0.05, 0.099, 0.1, 0.199, 0.2, 0.299, 0.3, 0.399, 0.4, 2, NA)
  expect_equal(classify_kernel_risk(x), c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, NA))
  cls <- kernel_risk_classes()
  expect_equal(cls$key, c("none", "low", "moderate", "high", "very_high", "extreme"))
  expect_equal(cls$lower, c(0, 0.05, 0.1, 0.2, 0.3, 0.4))
  expect_equal(cls$upper[1:5], c(0.05, 0.1, 0.2, 0.3, 0.4))
  expect_true(is.infinite(cls$upper[6]))
  # custom limits
  expect_equal(classify_kernel_risk(0.5, c(0.1, 0.2, 0.3, 0.4, 0.6)), 5)
})

test_that("invalid risk limits are rejected", {
  expect_error(validate_risk_thresholds(c(0.1, 0.2)), "5 increasing")
  expect_error(validate_risk_thresholds(c(0.1, 0.1, 0.2, 0.3, 0.4)), "5 increasing")
  expect_error(validate_risk_thresholds(c(0.3, 0.2, 0.4, 0.5, 0.6)), "5 increasing")
  expect_error(validate_risk_thresholds(c(0, 0.1, 0.2, 0.3, 0.4)), "5 increasing")
  expect_error(validate_risk_thresholds(c("a", "b", "c", "d", "e")), "5 increasing")
  expect_equal(validate_risk_thresholds(c("0.05", "0.1", "0.2", "0.3", "0.4")), default_risk_thresholds)
})

make_output <- function() {
  dates <- as.Date("2026-09-01") + 0:9
  data.frame(Date = dates, Blast_Kernel_Infection = c(0, 0.02, 0.06, 0.12, 0.25, 0.35, 0.33, 0.2, 0.15, 0.08))
}

test_that("summarize_kernel_risk gives peak, latest, first dates and the forecast window", {
  out <- make_output()
  fs <- list(summary = data.frame(
    Forecast_Start_Date = as.Date("2026-09-07"), Forecast_End_Date = as.Date("2026-09-10"),
    Kernel_Infection_At_Start = 0.33, Kernel_Infection_At_End = 0.08, Kernel_Infection_Gain = -0.25,
    Days_Infection_Conditions_Met = 2L))
  r <- summarize_kernel_risk(out, forecast_summary = fs)
  expect_equal(r$peak_value, 0.35)
  expect_equal(r$peak_date, as.Date("2026-09-06"))
  expect_equal(r$peak_level, 5)                      # 0.3 <= 0.35 < 0.4: very high
  expect_equal(r$latest_value, 0.08)
  expect_equal(r$latest_level, 2)                    # low
  expect_equal(r$first_dates[1], as.Date("2026-09-01"))   # none from the start
  expect_equal(r$first_dates[2], as.Date("2026-09-03"))   # first >= 0.05
  expect_equal(r$first_dates[5], as.Date("2026-09-06"))   # first >= 0.3
  expect_true(is.na(r$first_dates[6]))                    # never reached 0.4
  expect_equal(r$forecast$max_value, 0.33)
  expect_equal(r$forecast$max_level, 5)
  expect_equal(r$forecast$days_conditions, 2)
  expect_null(summarize_kernel_risk(out)$forecast)
})

test_that("report text exists in both languages with identical structure and placeholders", {
  skip_if_not_installed("yaml")
  path <- file.path(project_root, "report", "report_text.yaml")
  all_text <- yaml::read_yaml(path, fileEncoding = "UTF-8")
  expect_setequal(names(all_text), c("en", "pt"))
  shape <- function(x) if (is.list(x)) lapply(x, shape) else if (length(x) > 1) length(x) else "leaf"
  expect_equal(shape(all_text$en), shape(all_text$pt))
  expect_equal(length(all_text$en$recs), length(all_text$pt$recs))
  for (k in names(all_text$en$text)) {
    n <- function(s) lengths(regmatches(s, gregexpr("%s", s, fixed = TRUE)))
    expect_equal(n(all_text$en$text[[k]]), n(all_text$pt$text[[k]]), info = k)
  }
  expect_setequal(names(all_text$en$risk$labels), risk_keys)
  expect_setequal(names(all_text$en$risk$advice), risk_keys)
  expect_setequal(names(all_text$pt$risk$labels), risk_keys)
  expect_equal(load_report_text("pt", path)$risk$labels$none, all_text$pt$risk$labels$none)
})

test_that("the loader reads the report settings and rejects bad limits", {
  skip_if_not_installed("yaml")
  ok <- tempfile(fileext = ".yaml")
  writeLines(c("weather:", "  nasa_power_open_meteo: true",
               "report:", "  language: PT", "  author: Test Author", "  recipients: [A, B]",
               "  risk_thresholds: [0.1, 0.2, 0.3, 0.4, 0.5]"), ok)
  r <- load_yaml_configuration(ok)
  expect_equal(r$config$report_language, "pt")
  expect_equal(r$config$report_author, "Test Author")
  expect_equal(r$config$report_recipients, c("A", "B"))
  expect_equal(r$config$report_block_days, 21L)
  expect_equal(r$config$risk_thresholds, c(0.1, 0.2, 0.3, 0.4, 0.5))
  bad <- tempfile(fileext = ".yaml")
  writeLines(c("weather:", "  nasa_power_open_meteo: true",
               "report:", "  risk_thresholds: [0.3, 0.2, 0.1, 0.4, 0.5]"), bad)
  expect_error(load_yaml_configuration(bad), "increasing")
  blocks <- tempfile(fileext = ".yaml")
  writeLines(c("weather:", "  nasa_power_open_meteo: true", "report:", "  forecast_block_days: 0"), blocks)
  expect_error(load_yaml_configuration(blocks), "forecast_block_days")
  d <- load_yaml_configuration(file.path(project_root, "wheat_blast_model_config.yaml"))
  expect_equal(d$config$report_language, "en")
  expect_equal(d$config$risk_thresholds, default_risk_thresholds)
})

make_results <- function(with_forecast = TRUE) {
  dates <- as.Date("2026-09-01") + 0:9
  out <- make_output()
  out$Time <- 1:10
  out$Thermal_Time <- seq(900, 1500, length.out = 10)
  out$Blast_Airborne_Spores <- seq(1, 10)
  out$Blast_Kernel_Spores <- seq(0, 9)
  out$Leaf_Disease_Severity_Pct <- seq(0, 9)
  out$Infection_Temp_Factor <- rep(c(0.2, 0.8), 5)
  fs <- NULL
  if (with_forecast) {
    fs <- list(
      summary = data.frame(
        Forecast_Start_Date = dates[7], Forecast_End_Date = dates[10],
        Kernel_Infection_At_Start = 0.33, Kernel_Infection_At_End = 0.08, Kernel_Infection_Gain = -0.25,
        Days_Infection_Conditions_Met = 2L),
      daily = data.frame(
        Date = dates[7:10], Rain_mm = c(0, 3, 10, 0), Tmin = 10, Tmean = 15, Tmax = 21,
        RH_Wet_Hours = c(4, 9, 12, 6), Wet_Period_Met = c(FALSE, TRUE, TRUE, FALSE),
        Infection_Temp_Factor = c(0.2, 0.5, 0.8, 0.1), Heading_Susceptibility = 0.5,
        Kernel_Infection = c(0.33, 0.2, 0.15, 0.08),
        Infection_Conditions_Met = c(FALSE, TRUE, TRUE, FALSE)))
  }
  list(
    config = list(location_name = "Test Place, Country", latitude = -30.4, longitude = -56.47,
                  open_meteo_timezone = "America/Montevideo", weather_source = "NASA_POWER_OPEN_METEO",
                  start_date = as.Date("2026-06-26"), rh_wet_threshold_pct = 90),
    params = list(topsoil_fc = 0.35, topsoil_wp = 0.1, infection_min_wet_hours = 8,
                  leaf_Tmin = 15, leaf_Topt = 28, leaf_Tmax = 30),
    weather = data.frame(date = dates, rain = c(0, 5, 0, 2, 0, 0, 0, 3, 10, 0),
                         tmax = 22, tmin = 10, rh_wet_hours = c(3, 9, 10, 4, 8, 2, 4, 9, 12, 6),
                         weather_source = rep(c("NASA_POWER", "OPEN_METEO_FORECAST"), c(6, 4))),
    soil_water = data.frame(tday = 1:10, topsoil_theta = seq(0.2, 0.3, length.out = 10)),
    output = out,
    phenology = data.frame(Stage = c("Emergence", "Anthesis"), TT_Threshold = c(100, 1300),
                           Date = dates[c(1, 5)]),
    forecast_summary = fs
  )
}

test_that("the HTML report renders in English and Portuguese (needs rmarkdown, ggplot2, pandoc)", {
  skip_on_cran()
  for (pkg in c("yaml", "rmarkdown", "knitr", "ggplot2", "htmltools")) skip_if_not_installed(pkg)
  skip_if(!rmarkdown::pandoc_available(), "pandoc is not available")
  soil <- data.frame(thickness = c(0.2, 0.3), FC = c(0.35, 0.33), WP = c(0.1, 0.1))
  for (lang in c("en", "pt")) {
    for (with_fc in c(TRUE, FALSE)) {
      out <- tempfile(fileext = ".html")
      render_simulation_report(make_results(with_fc), out, language = lang, author = "A. Author",
                               recipients = c("R1", "R2"), cultivar = "Cv", susceptibility = 0.7,
                               soil_layers = soil)
      expect_true(file.exists(out), info = paste(lang, with_fc))
      html <- paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      expect_match(html, "A. Author", fixed = TRUE)
      expect_match(html, if (lang == "pt") "Muito alto" else "Very high", fixed = TRUE)
      if (with_fc) {   # the forecast is summarised; the days with all conditions met are listed
        expect_match(html, if (lang == "pt") "08/09/2026, 09/09/2026" else "2026-09-08, 2026-09-09", fixed = TRUE)
      }
    }
  }
})
