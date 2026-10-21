# ============================================================================
# 12_report.R
# Kernel-infection risk classes and the HTML simulation report.
# The report template is report/simulation_report.Rmd, its text (English and
# Portuguese) is report/report_text.yaml. Needs rmarkdown, knitr, ggplot2 and
# pandoc (included with RStudio) only when a report is rendered.
# ============================================================================

# --- Risk classes (kernel infection, relative units) ---------------------------

default_risk_thresholds <- c(0.05, 0.10, 0.20, 0.30, 0.40)
risk_keys <- c("none", "low", "moderate", "high", "very_high", "extreme")

validate_risk_thresholds <- function(thresholds) {
  th <- suppressWarnings(as.numeric(thresholds))
  if (length(th) != 5L || anyNA(th) || any(!is.finite(th)) || th[1] <= 0 || is.unsorted(th, strictly = TRUE)) {
    stop("Risk class limits need 5 increasing positive numbers, for example 0.05, 0.1, 0.2, 0.3, 0.4.",
         call. = FALSE)
  }
  th
}

# One row per class: level, key, lower bound (inclusive), upper bound, colors.
kernel_risk_classes <- function(thresholds = default_risk_thresholds, labels = NULL) {
  th <- validate_risk_thresholds(thresholds)
  data.frame(
    level = 1:6, key = risk_keys,
    label = if (is.null(labels)) risk_keys else unname(unlist(labels[risk_keys])),
    lower = c(0, th), upper = c(th, Inf),
    color = c("#D9D9D9", "#FFE08A", "#FDB863", "#F46D43", "#D73027", "#7F0000"),
    text_color = c("#222222", "#222222", "#222222", "#FFFFFF", "#FFFFFF", "#FFFFFF"),
    stringsAsFactors = FALSE
  )
}

# Class level (1-6) of kernel infection values; a value at a limit is in the higher class.
classify_kernel_risk <- function(x, thresholds = default_risk_thresholds) {
  th <- validate_risk_thresholds(thresholds)
  out <- findInterval(x, th) + 1L
  out[!is.finite(x)] <- NA_integer_
  out
}

# Peak, latest and forecast-window kernel infection with their classes, and the
# first day each class was reached.
summarize_kernel_risk <- function(output, thresholds = default_risk_thresholds,
                                  forecast_summary = NULL, labels = NULL) {
  classes <- kernel_risk_classes(thresholds, labels)
  value <- output$Blast_Kernel_Infection
  dates <- as.Date(output$Date)
  peak <- which.max(value)
  last <- length(value)
  first_dates <- as.Date(vapply(classes$level, function(l) {
    hit <- which(value >= classes$lower[l])
    if (length(hit)) format(dates[hit[1]]) else NA_character_
  }, character(1)))
  forecast <- NULL
  if (!is.null(forecast_summary)) {
    s <- forecast_summary$summary
    in_window <- dates >= s$Forecast_Start_Date
    window_max <- if (any(in_window)) max(value[in_window], na.rm = TRUE) else NA_real_
    forecast <- list(
      start = s$Forecast_Start_Date, end = s$Forecast_End_Date,
      value_start = s$Kernel_Infection_At_Start, value_end = s$Kernel_Infection_At_End,
      gain = s$Kernel_Infection_Gain, days_conditions = s$Days_Infection_Conditions_Met,
      max_value = window_max,
      max_level = classify_kernel_risk(window_max, thresholds)
    )
  }
  list(
    classes = classes, thresholds = validate_risk_thresholds(thresholds),
    peak_value = value[peak], peak_date = dates[peak],
    peak_level = classify_kernel_risk(value[peak], thresholds),
    latest_value = value[last], latest_date = dates[last],
    latest_level = classify_kernel_risk(value[last], thresholds),
    first_dates = first_dates, forecast = forecast
  )
}

# --- Report text and rendering -------------------------------------------------

locate_report_file <- function(name) {
  roots <- c(if (exists("project_root", inherits = TRUE)) get("project_root", inherits = TRUE),
             if (exists("script_directory", inherits = TRUE)) get("script_directory", inherits = TRUE),
             getwd(), dirname(getwd()))
  for (r in unique(roots)) {
    path <- file.path(r, "report", name)
    if (file.exists(path)) return(normalizePath(path))
  }
  stop("Report file not found: report/", name,
       ". Run from the project folder or keep the report/ folder next to R/.", call. = FALSE)
}

load_report_text <- function(language = "en", path = locate_report_file("report_text.yaml")) {
  all_text <- yaml::read_yaml(path, fileEncoding = "UTF-8")
  language <- if (identical(tolower(language), "pt")) "pt" else "en"
  txt <- all_text[[language]]
  if (is.null(txt)) stop("No text for language '", language, "' in ", path, call. = FALSE)
  txt
}

# Renders the HTML report for the result of run_simulation().
#   results      value returned by run_simulation()
#   output_file  path of the .html file to write
#   language     "en" or "pt"
#   soil_layers  data frame of the soil layers that were used
render_simulation_report <- function(results, output_file, language = "en",
                                     author = NULL, recipients = NULL,
                                     cultivar = NULL, susceptibility = NULL,
                                     soil_layers = soil_layers_df, thresholds = NULL,
                                     block_days = NULL, quiet = TRUE) {
  for (pkg in c("rmarkdown", "knitr", "ggplot2")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required for the report. Install it with install.packages('",
           pkg, "').", call. = FALSE)
    }
  }
  if (!rmarkdown::pandoc_available()) {
    stop("Pandoc is required to build the report. It is included with RStudio; otherwise ",
         "install it from https://pandoc.org/installing.html.", call. = FALSE)
  }
  thresholds <- thresholds %||% results$config$risk_thresholds %||% default_risk_thresholds
  language <- if (identical(tolower(language), "pt")) "pt" else "en"
  txt <- load_report_text(language)
  risk <- summarize_kernel_risk(results$output, thresholds, results$forecast_summary,
                                labels = txt$risk$labels)
  block_days <- as.integer(block_days %||% results$config$report_block_days %||% 21L)
  if (!is.finite(block_days) || block_days < 1L) stop("block_days must be a positive whole number.", call. = FALSE)
  meta <- list(language = language, author = author, recipients = recipients, block_days = block_days,
               cultivar = cultivar, susceptibility = susceptibility,
               soil_layers = soil_layers)
  # Render a copy in a scratch folder so no files appear next to the template.
  work <- tempfile("wb_report"); dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  template <- file.path(work, "simulation_report.Rmd")
  file.copy(locate_report_file("simulation_report.Rmd"), template)
  out_dir <- dirname(output_file)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir <- normalizePath(out_dir)
  rmarkdown::render(
    template, output_file = basename(output_file), output_dir = out_dir,
    params = list(results = results, text = txt, risk = risk, meta = meta),
    envir = new.env(parent = globalenv()), encoding = "UTF-8", quiet = quiet
  )
  invisible(normalizePath(file.path(out_dir, basename(output_file))))
}
