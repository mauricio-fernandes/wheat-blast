# Helpers for the Shiny dashboard: input parsing/validation, the settings builder
# (dashboard inputs -> the same structure as the YAML file), YAML export and a
# wrapper that runs the model while capturing messages and warnings.
#
# Needs the model modules (R/01_utils.R for %||%, and for run_model: R/02_params.R
# and the rest) to be sourced first, and the yaml package.

# --- Parsing -------------------------------------------------------------------

# "0.35, 0.33; 0.31 0.27" -> c(0.35, 0.33, 0.31, 0.27)
parse_numeric_vector <- function(text, n = NULL, name = "Values") {
  parts <- unlist(strsplit(as.character(text %||% ""), "[,;[:space:]]+"))
  parts <- parts[nzchar(parts)]
  values <- suppressWarnings(as.numeric(parts))
  if (!length(values) || anyNA(values)) {
    stop(name, " must be numbers separated by commas.", call. = FALSE)
  }
  if (!is.null(n) && length(values) != n) {
    stop(name, " needs exactly ", n, " values (one per soil layer); got ",
         length(values), ".", call. = FALSE)
  }
  values
}

# Free-text "name: value" lines -> named list (NULL when empty). Names must be
# parameters of the model; unknown names are reported by the model itself.
parse_overrides <- function(text) {
  text <- trimws(text %||% "")
  if (!nzchar(text)) return(NULL)
  parsed <- tryCatch(
    yaml::yaml.load(text),
    error = function(e) {
      stop("Overrides are not valid YAML: ", conditionMessage(e), call. = FALSE)
    }
  )
  if (is.null(parsed)) return(NULL)
  if (!is.list(parsed) || is.null(names(parsed)) || any(!nzchar(names(parsed)))) {
    stop("Overrides must be 'name: value' lines, for example ",
         "'leaf_infection_rate: 0.3'.", call. = FALSE)
  }
  parsed
}

prune_null <- function(x) {
  if (!is.list(x)) return(x)
  x <- x[!vapply(x, is.null, logical(1))]
  lapply(x, prune_null)
}

# --- Settings builder ----------------------------------------------------------

# `v` is a plain named list of dashboard values (see collect_inputs() in app.R).
# Returns list(yaml = <list with the same layout as the YAML file>,
#              weather_mode, csv_path, notes). Stops with a readable message on
# invalid input, so the app can show it instead of failing inside the model.
build_settings <- function(v, n_layers = 4L) {
  num <- function(x, name, min = -Inf, max = Inf) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) != 1L || !is.finite(x)) {
      stop(name, " must be a number.", call. = FALSE)
    }
    if (x < min || x > max) {
      stop(name, " must be between ", min, " and ", max, ".", call. = FALSE)
    }
    x
  }
  as_date <- function(x, name) {
    d <- tryCatch(as.Date(x), error = function(e) as.Date(NA))
    if (length(d) != 1L || is.na(d)) stop(name, " must be a valid date.", call. = FALSE)
    d
  }
  text <- function(x, default) {
    x <- trimws(as.character(x %||% ""))
    if (length(x) != 1L || !nzchar(x)) default else x
  }
  notes <- character()

  mode <- v$weather_mode %||% "blended"
  if (!mode %in% c("blended", "nasa", "csv")) {
    stop("Weather source must be NASA POWER + Open-Meteo, NASA POWER only, or CSV.",
         call. = FALSE)
  }

  latitude <- num(v$latitude, "Latitude", -90, 90)
  longitude <- num(v$longitude, "Longitude", -180, 180)
  sowing <- as_date(v$sowing_date, "Sowing date")
  susceptibility <- num(v$susceptibility, "Cultivar susceptibility", 0, 1)

  tt <- c(
    num(v$TT_FlagLeaf, "Thermal time to flag leaf", 100),
    num(v$TT_Anthesis, "Thermal time to anthesis"),
    num(v$TT_GrainFill, "Thermal time to end of grain fill"),
    num(v$TTM, "Thermal time to maturity")
  )
  if (is.unsorted(tt, strictly = TRUE)) {
    stop("Thermal-time thresholds must increase: flag leaf < anthesis < ",
         "grain fill < maturity.", call. = FALSE)
  }

  heading <- c(
    num(v$spike_start, "Heading window start (thermal time)", 0),
    num(v$spike_peak, "Heading window peak (thermal time)", 0),
    num(v$spike_end, "Heading window end (thermal time)", 0)
  )
  if (is.unsorted(heading, strictly = TRUE)) {
    stop("Heading window must satisfy start < peak < end.", call. = FALSE)
  }
  kernel_start <- num(v$kernel_start, "Kernel formation thermal time", 0)
  hard_dough <- num(v$hard_dough, "Hard dough thermal time", 0)
  if (kernel_start >= hard_dough) {
    stop("Kernel formation must come before hard dough.", call. = FALSE)
  }

  rh_threshold <- num(v$rh_threshold %||% 90, "Relative-humidity threshold", 1, 100)
  min_wet_hours <- num(v$min_wet_hours %||% 8, "Minimum wet period", 0, 24)
  inf_t <- c(
    num(v$inf_tmin %||% 15, "Infection minimum temperature", -10, 50),
    num(v$inf_topt %||% 28, "Infection optimum temperature", -10, 50),
    num(v$inf_tmax %||% 30, "Infection maximum temperature", -10, 50)
  )
  if (is.unsorted(inf_t, strictly = TRUE)) {
    stop("Infection temperatures must satisfy minimum < optimum < maximum.",
         call. = FALSE)
  }

  alt_lai <- num(v$alt_lai, "Alternative host LAI", 0)
  alt_area <- num(v$alt_area, "Alternative host initial infectious area", 0)
  alt_mult <- num(v$alt_mult, "Alternative host spore multiplier", 0)
  if (isTRUE(v$alt_enabled) && alt_area > alt_lai) {
    stop("Initial infectious area cannot exceed the alternative host LAI.",
         call. = FALSE)
  }

  lead_days <- num(v$lead_days, "Pre-sowing lead days", 0, 365)
  residue <- num(v$residue, "Initial residue inoculum", 0)
  time_step <- num(v$time_step, "Time step", 0.02, 1)
  forecast_days <- as.integer(round(num(v$forecast_days %||% 16, "Forecast days", 1, 16)))
  leaf_max_frac <- num(v$leaf_max_frac, "Maximum diseased leaf fraction", 0, 1)
  leaf_lesion <- num(v$leaf_lesion, "Maximum lesion size", 1e-9)
  leaf_crowding <- num(v$leaf_crowding, "Crowding exponent", 1)

  FC <- parse_numeric_vector(v$soil_FC, n_layers, "Field capacity (FC)")
  WP <- parse_numeric_vector(v$soil_WP, n_layers, "Wilting point (WP)")
  SAT <- parse_numeric_vector(v$soil_SAT, n_layers, "Saturation (SAT)")
  if (any(c(FC, WP, SAT) <= 0 | c(FC, WP, SAT) >= 1)) {
    stop("Soil water contents must be fractions between 0 and 1.", call. = FALSE)
  }
  if (any(WP >= FC) || any(FC > SAT)) {
    stop("Soil layers must satisfy WP < FC <= SAT.", call. = FALSE)
  }
  overrides <- parse_overrides(v$overrides)

  csv_path <- v$csv_path
  if (mode == "csv") {
    if (is.null(csv_path) || !length(csv_path) || !file.exists(csv_path)) {
      stop("Upload a weather CSV with columns date, rain, tmax, tmin, dpt, srad, rh_wet_hours.",
           call. = FALSE)
    }
    notes <- c(notes, "CSV mode: the file must contain date, rain, tmax, tmin, dpt, srad, rh_wet_hours.")
  } else {
    csv_path <- NULL
  }

  reference_date <- NULL
  if (isTRUE(v$hindcast)) {
    if (mode == "blended") {
      stop("Hindcast needs 'NASA POWER only' or 'CSV file'; the forecast source ",
           "always starts from today.", call. = FALSE)
    }
    reference_date <- as_date(v$hindcast_date, "Hindcast end date")
    if (reference_date < sowing) {
      stop("The hindcast end date must be on or after the sowing date.", call. = FALSE)
    }
    if (reference_date > Sys.Date()) {
      stop("The hindcast end date cannot be in the future.", call. = FALSE)
    }
  }

  yaml_list <- list(
    location = list(
      name = text(v$location_name, "unspecified"),
      latitude = latitude, longitude = longitude,
      timezone = text(v$timezone, "auto")
    ),
    crop = list(
      sowing_date = format(sowing),
      cultivar = text(v$cultivar, "unspecified"),
      cultivar_susceptibility = susceptibility,
      TT_Anthesis = tt[2], TT_FlagLeaf = tt[1], TT_GrainFill = tt[3], TTM = tt[4]
    ),
    weather = list(
      nasa_power_only = mode != "blended",
      nasa_power_open_meteo = mode == "blended",
      reference_date = if (is.null(reference_date)) NULL else format(reference_date),
      nasa_community = text(v$nasa_community, "AG"),
      nasa_time_standard = text(v$nasa_time_standard, "LST"),
      rh_wet_threshold_pct = rh_threshold,
      open_meteo_past_days = 92L,
      open_meteo_forecast_days = forecast_days,
      csv_file = csv_path
    ),
    simulation = list(
      time_step = time_step,
      make_plots = FALSE,
      stop_at_maturity = isTRUE(v$stop_at_maturity),
      spore_dynamics_lead_days = lead_days,
      blast_initial_residue_inoculum = residue,
      leaf_initial_inoculum_size = 0
    ),
    alternative_host = list(
      enabled = isTRUE(v$alt_enabled), lai = alt_lai,
      initial_infectious_area = alt_area, spore_production_multiplier = alt_mult
    ),
    soil = list(FC = as.list(FC), WP = as.list(WP), SAT = as.list(SAT)),
    pathogen = list(
      spike_heading_start_TT = heading[1], spike_heading_peak_TT = heading[2],
      spike_heading_end_TT = heading[3],
      kernel_start_TT = kernel_start, hard_dough_TT = hard_dough,
      leaf_max_diseased_fraction = leaf_max_frac, leaf_max_lesion_size = leaf_lesion,
      leaf_infection_crowding_exponent = leaf_crowding,
      infection_min_wet_hours = min_wet_hours,
      infection_Tmin_C = inf_t[1], infection_Topt_C = inf_t[2],
      infection_Tmax_C = inf_t[3]
    ),
    overrides = overrides
  )
  list(yaml = yaml_list, weather_mode = mode, csv_path = csv_path, notes = notes)
}

# YAML text in the layout of wheat_blast_model_config.yaml (true/false, not yes/no).
settings_to_yaml <- function(settings) {
  body <- yaml::as.yaml(
    prune_null(settings$yaml),
    handlers = list(logical = yaml::verbatim_logical)
  )
  paste0(
    "# Wheat blast forecast configuration (exported from the dashboard)\n",
    "# Temperatures are degrees C; rainfall is mm/day; radiation is MJ/m2/day.\n",
    body
  )
}

# --- Running the model ---------------------------------------------------------

# Runs the model for `settings` (from build_settings()). Messages, warnings and
# printed output are collected in `log` instead of going to the console. Never
# throws: failures come back in `error`. Needs all model modules sourced.
run_model <- function(settings, yaml_path = tempfile(fileext = ".yaml")) {
  log <- character()
  add <- function(x) log <<- c(log, x)
  writeLines(settings_to_yaml(settings), yaml_path)
  printed <- utils::capture.output({
    outcome <- withCallingHandlers(
      tryCatch({
        loaded <- load_yaml_configuration(yaml_path)
        config <- loaded$config
        if (identical(settings$weather_mode, "csv")) {
          config$weather_source <- "CSV"
          config$weather_file_path <- settings$csv_path
        }
        run_simulation(
          config = config, params = loaded$params, soil_layers = loaded$soil
        )
      }, error = function(e) e),
      message = function(m) {
        add(trimws(conditionMessage(m)))
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        add(paste("WARNING:", conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    )
  })
  failed <- inherits(outcome, "error")
  list(
    result = if (failed) NULL else outcome,
    error = if (failed) conditionMessage(outcome) else NULL,
    log = c(log, printed[nzchar(trimws(printed))])
  )
}
