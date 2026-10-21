# ============================================================================
# 04_weather.R
# Weather: NASA POWER, Open-Meteo, blending, gap filling, preprocessing.
# ============================================================================

# --- 2. Weather data processing --------------------------------------------

parse_weather_dates <- function(date_values) {
  date_text <- trimws(as.character(date_values))
  parsed <- as.Date(rep(NA_character_, length(date_text)))

  # Five-digit Julian format YYDDD:
  #   15001 = year 2015, day 001 = 2015-01-01
  #   15365 = year 2015, day 365 = 2015-12-31
  julian_idx <- !is.na(date_text) & grepl("^[0-9]{5}$", date_text)
  if (any(julian_idx)) {
    yy <- as.integer(substr(date_text[julian_idx], 1, 2))
    day_of_year <- as.integer(substr(date_text[julian_idx], 3, 5))
    full_year <- ifelse(yy <= 68, 2000 + yy, 1900 + yy)
    year_start <- as.Date(paste0(full_year, "-01-01"))
    julian_dates <- year_start + day_of_year - 1

    # Reject day 000 and values that spill into the following year.
    valid_julian <- day_of_year >= 1 &
      format(julian_dates, "%Y") == as.character(full_year)
    julian_dates[!valid_julian] <- as.Date(NA)
    parsed[julian_idx] <- julian_dates
  }

  formats <- c("%m/%d/%y", "%m/%d/%Y", "%Y-%m-%d",
               "%d/%m/%y", "%d/%m/%Y")
  for (fmt in formats) {
    missing <- is.na(parsed) & !is.na(date_text) & date_text != ""
    if (!any(missing)) break
    parsed[missing] <- as.Date(date_text[missing], format = fmt)
  }
  parsed
}

impute_short_weather_gaps <- function(values, dates, variable,
                                      max_gap_days = 3L) {
  values <- as.numeric(values)
  missing <- !is.finite(values) | values <= -900
  values[missing] <- NA_real_
  if (all(is.na(values))) {
    stop("Weather variable ", variable,
         " contains no usable observations.", call. = FALSE)
  }
  if (sum(!is.na(values)) < 2) {
    stop("Weather variable ", variable,
         " needs at least two usable observations for gap filling.",
         call. = FALSE)
  }
  runs <- rle(is.na(values))
  run_ends <- cumsum(runs$lengths)
  run_starts <- run_ends - runs$lengths + 1L
  missing_runs <- which(runs$values)
  for (run_id in missing_runs) {
    idx <- seq.int(run_starts[run_id], run_ends[run_id])
    if (length(idx) > max_gap_days) {
      stop(
        "Missing weather gap for ", variable, " from ",
        format(dates[min(idx)]), " to ", format(dates[max(idx)]),
        " exceeds the configured short-gap limit of ", max_gap_days,
        " days. Use another source or correct the input data.",
        call. = FALSE
      )
    }
    observed <- which(!is.na(values))
    # Linear interpolation for internal gaps; nearest observed value for a
    # short leading/trailing gap. All filled cells remain explicitly flagged.
    values[idx] <- approx(
      x = observed, y = values[observed], xout = idx,
      method = "linear", rule = 2, ties = "ordered"
    )$y
  }
  list(values = values, imputed = missing)
}

preprocess_weather_data <- function(weather, start_date, end_date) {
  required <- c("rain", "tmax", "tmin", "dpt", "srad", "date")
  if (!all(required %in% names(weather))) {
    stop("Weather data must contain: ", paste(required, collapse = ", "),
         call. = FALSE)
  }
  weather$date <- if (inherits(weather$date, "Date")) weather$date else
    parse_weather_dates(weather$date)
  if (all(is.na(weather$date))) {
    stop("All dates are NA after parsing.", call. = FALSE)
  }
  # Daily hours with RH at or above the wet threshold (the wet-period variable
  # that drives infection) come from the hourly weather, or from a CSV column of
  # the same name.
  if (!"rh_wet_hours" %in% names(weather)) {
    stop("Weather data need a daily rh_wet_hours column (hours per day with ",
         "relative humidity at or above the wet threshold). It is computed from ",
         "hourly NASA POWER / Open-Meteo data; a CSV must include it.",
         call. = FALSE)
  }
  weather$rh_wet_hours <- suppressWarnings(as.numeric(weather$rh_wet_hours))
  required <- c(required, "rh_wet_hours")
  weather <- weather[!is.na(weather$date) & weather$date >= start_date &
                        weather$date <= end_date, required]
  weather <- weather[order(weather$date), ]
  if (nrow(weather) < 2) {
    stop("At least two weather rows are required in the simulation period.",
         call. = FALSE)
  }
  if (anyDuplicated(weather$date)) {
    stop("Weather data contain duplicate dates.", call. = FALSE)
  }
  # Restore omitted calendar rows as NA so isolated missing dates can be
  # handled by the same transparent short-gap logic as NASA's -999 values.
  full_dates <- seq(min(weather$date), max(weather$date), by = "day")
  weather <- weather[match(full_dates, weather$date), , drop = FALSE]
  weather$date <- full_dates
  variables <- c("tmax", "tmin", "dpt", "rain", "srad", "rh_wet_hours")
  imputation_flags <- list()
  for (col in variables) {
    filled <- impute_short_weather_gaps(
      weather[[col]], weather$date, col, max_gap_days = 3L
    )
    weather[[col]] <- filled$values
    imputation_flags[[paste0("imputed_", col)]] <- filled$imputed
  }
  for (col in c("rain", "srad")) weather[[col]] <- pmax(0, weather[[col]])
  weather$rh_wet_hours <- pmin(24, pmax(0, weather$rh_wet_hours))
  for (flag_name in names(imputation_flags)) {
    weather[[flag_name]] <- imputation_flags[[flag_name]]
  }
  flag_names <- names(imputation_flags)
  weather$weather_imputed <- Reduce(`|`, weather[flag_names])
  weather$imputed_variables <- vapply(seq_len(nrow(weather)), function(i) {
    filled_cols <- sub(
      "^imputed_", "", flag_names[
        vapply(weather[flag_names], `[`, logical(1), i)
      ]
    )
    paste(filled_cols, collapse = ",")
  }, character(1))
  if (any(weather$weather_imputed)) {
    warning(
      "Interpolated/edge-filled short weather gaps (maximum 3 consecutive days). ",
      "Affected dates: ",
      paste(format(weather$date[weather$weather_imputed]), collapse = ", "),
      ". See imputed_variables in the returned weather data.",
      call. = FALSE
    )
  }
  if (any(weather$tmax < weather$tmin)) {
    stop("Some weather rows have tmax < tmin.", call. = FALSE)
  }
  weather$tmean <- (weather$tmax + weather$tmin) / 2
  weather$eto <- 0.0023 * (weather$tmean + 17.8) *
    sqrt(pmax(0, weather$tmax - weather$tmin)) * (weather$srad / 2.45)
  weather$eto[!is.finite(weather$eto)] <- 0
  weather
}

load_and_preprocess_weather <- function(file_path, start_date, end_date) {
  if (!file.exists(file_path)) stop("Weather file not found: ", file_path,
                                     call. = FALSE)
  raw <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
  preprocess_weather_data(raw, start_date, end_date)
}

# Downloads a JSON document with a longer timeout, up to 3 attempts and the real
# reason on failure (download.file() only says "cannot open URL"; the cause, such
# as an HTTP status, is in its warning).
download_json_with_retry <- function(url, label, attempts = 3L, timeout = 120) {
  old_options <- options(timeout = max(timeout, getOption("timeout")))
  on.exit(options(old_options), add = TRUE)
  reason <- "unknown error"
  for (attempt in seq_len(attempts)) {
    response_file <- tempfile(fileext = ".json")
    warning_text <- character()
    status <- tryCatch(
      withCallingHandlers(
        utils::download.file(url, response_file, mode = "wb", quiet = TRUE,
                             method = "libcurl"),
        warning = function(w) {
          warning_text <<- c(warning_text, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        reason <<- paste(c(conditionMessage(e), warning_text), collapse = " | ")
        NULL
      }
    )
    if (!is.null(status) && file.exists(response_file) &&
        file.info(response_file)$size > 0) {
      return(response_file)
    }
    unlink(response_file)
    if (attempt < attempts) Sys.sleep(2 * attempt)
  }
  stop(label, " request failed after ", attempts, " attempts: ", reason,
       call. = FALSE)
}

# --- Hourly weather -> daily values --------------------------------------------

# Summarises hourly weather to one row per calendar day.
#   hourly: data.frame with date (Date), temp (C), rh (%), dpt (C), rain (mm/h)
#           and srad (W m-2, mean over the hour).
#   tmax/tmin = daily extremes of hourly temperature; dpt = daily mean dew point;
#   rain = sum of hourly rain; srad = MJ m-2 d-1 (hourly W m-2 x 3600 s);
#   rh_wet_hours = hours with RH >= rh_threshold (equal or higher), a proxy for
#   leaf wetness duration. If a day has some hours missing, rh_wet_hours is
#   scaled to 24 h (count x 24 / valid hours). A variable with fewer than
#   `min_hours` valid hours in a day is NA for that day (for example today's
#   partial NASA POWER day), so such days are not treated as complete.
aggregate_hourly_weather <- function(hourly, rh_threshold = 90, min_hours = 20L) {
  needed <- c("date", "temp", "rh", "dpt", "rain", "srad")
  if (!all(needed %in% names(hourly))) {
    stop("Hourly weather must contain: ", paste(needed, collapse = ", "),
         call. = FALSE)
  }
  hourly <- hourly[!is.na(hourly$date), , drop = FALSE]
  empty <- data.frame(date = as.Date(character()), tmax = numeric(),
                      tmin = numeric(), dpt = numeric(), rain = numeric(),
                      srad = numeric(), rh_wet_hours = numeric())
  if (!nrow(hourly)) return(empty)
  by_day <- split(hourly, hourly$date)
  rows <- lapply(by_day, function(h) {
    enough <- function(x) sum(is.finite(x)) >= min_hours
    valid_rh <- sum(is.finite(h$rh))
    data.frame(
      date = h$date[1],
      tmax = if (enough(h$temp)) max(h$temp, na.rm = TRUE) else NA_real_,
      tmin = if (enough(h$temp)) min(h$temp, na.rm = TRUE) else NA_real_,
      dpt = if (enough(h$dpt)) mean(h$dpt, na.rm = TRUE) else NA_real_,
      rain = if (enough(h$rain)) sum(h$rain, na.rm = TRUE) else NA_real_,
      srad = if (enough(h$srad)) sum(h$srad, na.rm = TRUE) * 3600 / 1e6 else NA_real_,
      rh_wet_hours = if (valid_rh >= min_hours) {
        min(24, sum(h$rh >= rh_threshold, na.rm = TRUE) * 24 / valid_rh)
      } else NA_real_
    )
  })
  daily <- do.call(rbind, rows)
  rownames(daily) <- NULL
  daily <- daily[order(daily$date), , drop = FALSE]
  if (any(daily$srad > 45, na.rm = TRUE)) {
    warning("Daily solar radiation above 45 MJ m-2 d-1 after summing hourly ",
            "values; check the radiation units of the hourly source.",
            call. = FALSE)
  }
  daily
}

nasa_response_dates <- function(response) {
  if (all(c("YEAR", "MO", "DY") %in% names(response))) {
    return(as.Date(sprintf("%04d-%02d-%02d", as.integer(response$YEAR),
                           as.integer(response$MO), as.integer(response$DY))))
  }
  if (all(c("YEAR", "MM", "DD") %in% names(response))) {
    return(as.Date(sprintf("%04d-%02d-%02d", as.integer(response$YEAR),
                           as.integer(response$MM), as.integer(response$DD))))
  }
  if ("YYYYMMDD" %in% names(response)) {
    value <- response$YYYYMMDD
    return(if (inherits(value, "Date")) value else
      as.Date(as.character(value), format = "%Y%m%d"))
  }
  if ("DATE" %in% names(response)) return(parse_weather_dates(response$DATE))
  stop("nasapower response has no recognizable date fields.", call. = FALSE)
}

# Hourly NASA POWER (T2M, RH2M, T2MDEW, PRECTOTCORR, ALLSKY_SFC_SW_DWN) summarised
# to daily values. Days are the calendar days of the chosen time standard; with
# "LST" that is local SOLAR time (a few tens of minutes off civil time).
fetch_nasa_power_hourly <- function(latitude, longitude, start_date, end_date,
                                    community = "AG", time_standard = "LST",
                                    rh_threshold = 90) {
  if (!is.finite(latitude) || latitude < -90 || latitude > 90 ||
      !is.finite(longitude) || longitude < -180 || longitude > 180) {
    stop("NASA POWER latitude/longitude are invalid.", call. = FALSE)
  }
  if (start_date < as.Date("2001-01-01")) {
    stop("NASA POWER hourly data begin in 2001; requested start is earlier.",
         call. = FALSE)
  }
  if (identical(toupper(time_standard), "UTC")) {
    message("NASA POWER hourly data use UTC days; daily values will not match ",
            "local calendar days. LST is recommended.")
  }
  parameters <- c("T2M", "RH2M", "T2MDEW", "PRECTOTCORR", "ALLSKY_SFC_SW_DWN")
  numeric_value <- function(x) {
    value <- suppressWarnings(as.numeric(as.character(x)))
    value[!is.finite(value) | value <= -900] <- NA_real_
    value
  }
  month_starts <- seq(as.Date(format(start_date, "%Y-%m-01")),
                      as.Date(format(end_date, "%Y-%m-01")), by = "month")
  pieces <- vector("list", length(month_starts))
  for (i in seq_along(month_starts)) {
    chunk_start <- max(start_date, month_starts[i])
    month_after <- as.Date(format(month_starts[i] + 35, "%Y-%m-01"))
    chunk_end <- min(end_date, month_after - 1)
    response <- tryCatch(
      nasapower::get_power(
        community = community, pars = parameters,
        lonlat = c(longitude, latitude), temporal_api = "hourly",
        dates = c(format(chunk_start, "%Y-%m-%d"), format(chunk_end, "%Y-%m-%d")),
        time_standard = time_standard
      ),
      error = function(e) stop(
        "nasapower::get_power() (hourly) failed for ", chunk_start, " to ",
        chunk_end, ": ", conditionMessage(e), call. = FALSE
      )
    )
    response <- as.data.frame(response)
    names(response) <- toupper(names(response))
    missing_columns <- setdiff(parameters, names(response))
    if (length(missing_columns)) {
      stop("nasapower hourly response lacks column(s) ",
           paste(missing_columns, collapse = ", "), " for ", chunk_start,
           " to ", chunk_end, ".", call. = FALSE)
    }
    pieces[[i]] <- data.frame(
      date = nasa_response_dates(response),
      temp = numeric_value(response$T2M), rh = numeric_value(response$RH2M),
      dpt = numeric_value(response$T2MDEW),
      rain = numeric_value(response$PRECTOTCORR),
      srad = numeric_value(response$ALLSKY_SFC_SW_DWN)
    )
  }
  hourly <- do.call(rbind, pieces)
  hourly <- hourly[!is.na(hourly$date) & hourly$date >= start_date &
                     hourly$date <= end_date, , drop = FALSE]
  daily <- aggregate_hourly_weather(hourly, rh_threshold)
  last_with <- function(column) {
    ok <- is.finite(daily[[column]])
    if (any(ok)) format(max(daily$date[ok])) else "none"
  }
  message("NASA POWER hourly, last day with data: temperature ", last_with("tmax"),
          ", humidity ", last_with("rh_wet_hours"), ", rain ", last_with("rain"),
          ", radiation ", last_with("srad"), ".")
  message("NASA POWER hourly: ", nrow(hourly), " hours summarised to ",
          nrow(daily), " days (",
          if (nrow(daily)) format(min(daily$date)) else "no dates", " to ",
          if (nrow(daily)) format(max(daily$date)) else "no dates", ").")
  daily
}

open_meteo_hourly_variables <- c(
  "temperature_2m", "relative_humidity_2m", "dew_point_2m",
  "precipitation", "shortwave_radiation"
)

parse_open_meteo_hourly <- function(payload, label, rh_threshold) {
  if (!is.null(payload$error) && isTRUE(payload$error)) {
    stop(label, " API error: ", payload$reason %||% "unknown error",
         call. = FALSE)
  }
  hourly <- payload$hourly
  required <- c("time", open_meteo_hourly_variables)
  if (is.null(hourly) || !all(required %in% names(hourly))) {
    stop(label, " response is missing required hourly values.", call. = FALSE)
  }
  time_text <- as.character(hourly$time)  # "2026-10-16T13:00", local time
  aggregate_hourly_weather(
    data.frame(
      date = as.Date(substr(time_text, 1, 10)),
      temp = as.numeric(hourly$temperature_2m),
      rh = as.numeric(hourly$relative_humidity_2m),
      dpt = as.numeric(hourly$dew_point_2m),
      rain = as.numeric(hourly$precipitation),
      srad = as.numeric(hourly$shortwave_radiation)
    ),
    rh_threshold
  )
}

# Open-Meteo recent + forecast hours, summarised to days (local time zone).
fetch_open_meteo_hourly <- function(latitude, longitude,
                                    past_days = 92, forecast_days = 16,
                                    timezone = "auto", rh_threshold = 90) {
  query <- paste0(
    "latitude=", format(latitude, scientific = FALSE),
    "&longitude=", format(longitude, scientific = FALSE),
    "&hourly=", paste(open_meteo_hourly_variables, collapse = ","),
    "&temperature_unit=celsius&precipitation_unit=mm",
    "&past_days=", as.integer(past_days),
    "&forecast_days=", as.integer(forecast_days),
    "&timezone=", utils::URLencode(timezone, reserved = TRUE)
  )
  response_file <- download_json_with_retry(
    paste0("https://api.open-meteo.com/v1/forecast?", query), "Open-Meteo"
  )
  on.exit(unlink(response_file), add = TRUE)
  parse_open_meteo_hourly(
    jsonlite::fromJSON(response_file, simplifyVector = TRUE),
    "Open-Meteo", rh_threshold
  )
}

fetch_open_meteo_archive_hourly <- function(latitude, longitude,
                                            start_date, end_date,
                                            timezone = "auto",
                                            rh_threshold = 90) {
  if (start_date > end_date) {
    return(data.frame(date = as.Date(character()), tmax = numeric(),
                      tmin = numeric(), dpt = numeric(), rain = numeric(),
                      srad = numeric(), rh_wet_hours = numeric()))
  }
  query <- paste0(
    "latitude=", format(latitude, scientific = FALSE),
    "&longitude=", format(longitude, scientific = FALSE),
    "&start_date=", format(start_date, "%Y-%m-%d"),
    "&end_date=", format(end_date, "%Y-%m-%d"),
    "&hourly=", paste(open_meteo_hourly_variables, collapse = ","),
    "&temperature_unit=celsius&precipitation_unit=mm",
    "&timezone=", utils::URLencode(timezone, reserved = TRUE)
  )
  response_file <- download_json_with_retry(
    paste0("https://archive-api.open-meteo.com/v1/archive?", query),
    "Open-Meteo Archive"
  )
  on.exit(unlink(response_file), add = TRUE)
  parse_open_meteo_hourly(
    jsonlite::fromJSON(response_file, simplifyVector = TRUE),
    "Open-Meteo Archive", rh_threshold
  )
}

# Where the Open-Meteo archive must bridge NASA POWER and Open-Meteo's recent
# data. The recent/forecast endpoint can return rows with null values for older
# days (for example the first weeks of past_days), so the bridge runs up to the
# first Open-Meteo day with complete core values, not merely its first row.
find_weather_bridge <- function(nasa_last_available, meteo,
                                core_cols = c("tmax", "tmin", "dpt", "rain", "srad")) {
  usable <- meteo$date[complete.cases(meteo[, core_cols, drop = FALSE]) &
                         !is.na(meteo$date)]
  usable <- usable[usable > nasa_last_available]
  first_usable <- if (length(usable)) min(usable) else as.Date(NA)
  list(
    first_usable = first_usable,
    archive_start = nasa_last_available + 1,
    archive_end = if (is.na(first_usable)) as.Date(NA) else first_usable - 1
  )
}

fetch_blended_weather <- function(latitude, longitude, weather_start_date,
                                  reference_date = Sys.Date(),
                                  nasa_community = "AG",
                                  nasa_time_standard = "LST",
                                  open_meteo_timezone = "auto",
                                  open_meteo_past_days = 92,
                                  open_meteo_forecast_days = 16,
                                  rh_threshold = 90) {
  if (weather_start_date > reference_date) {
    stop("weather_start_date cannot be in the future.", call. = FALSE)
  }
  nasa_raw <- fetch_nasa_power_hourly(
    latitude, longitude, weather_start_date, reference_date,
    community = nasa_community, time_standard = nasa_time_standard,
    rh_threshold = rh_threshold
  )
  # A day counts as complete when the core variables are present; the humidity
  # hours are gap-filled in preprocess_weather_data() like the other variables.
  weather_cols <- c("tmax", "tmin", "dpt", "rain", "srad")
  output_cols <- c(weather_cols, "rh_wet_hours")
  nasa_good <- complete.cases(nasa_raw[, weather_cols]) &
    apply(nasa_raw[, weather_cols, drop = FALSE], 1,
          function(row) all(is.finite(row) & row > -900))
  nasa_observed <- nasa_raw[nasa_good, c("date", output_cols)]
  if (nrow(nasa_observed) == 0) {
    stop("NASA POWER returned no complete daily weather rows.", call. = FALSE)
  }
  nasa_last_available <- max(nasa_observed$date)
  # Retain partial NASA rows (for example, a missing radiation value) through
  # the last complete day. Preprocessing can then fill only the missing cells.
  nasa_observed <- nasa_raw[
    nasa_raw$date <= nasa_last_available, c("date", output_cols),
    drop = FALSE
  ]

  meteo <- fetch_open_meteo_hourly(
    latitude, longitude, past_days = open_meteo_past_days,
    forecast_days = open_meteo_forecast_days, timezone = open_meteo_timezone,
    rh_threshold = rh_threshold
  )
  if (!nrow(meteo) || all(is.na(meteo$date))) {
    stop("Open-Meteo returned no dated daily weather rows.", call. = FALSE)
  }
  weather_cols_core <- c("tmax", "tmin", "dpt", "rain", "srad")
  bridge <- find_weather_bridge(nasa_last_available, meteo, weather_cols_core)
  if (is.na(bridge$first_usable)) {
    stop("Open-Meteo returned no complete daily weather after NASA POWER's ",
         "last complete date (", format(nasa_last_available), ").", call. = FALSE)
  }
  message("NASA POWER is complete through ", format(nasa_last_available),
          "; Open-Meteo recent/forecast data are complete from ",
          format(bridge$first_usable), ".")
  # If Open-Meteo's recent/forecast data start after NASA's last fully
  # observed day, use its archive only for the intervening dates.
  archive_start <- bridge$archive_start
  archive_end <- bridge$archive_end
  archive_bridge <- if (archive_start <= archive_end) {
    fetch_open_meteo_archive_hourly(
      latitude, longitude, archive_start, archive_end,
      timezone = open_meteo_timezone, rh_threshold = rh_threshold
    )
  } else {
    data.frame(date = as.Date(character()), tmax = numeric(),
               tmin = numeric(), dpt = numeric(), rain = numeric(),
               srad = numeric(), rh_wet_hours = numeric())
  }
  # Keep NASA POWER through its last complete day. Use Open-Meteo only after
  # that day, discarding its overlapping recent-history rows.
  meteo <- meteo[meteo$date > nasa_last_available &
                   meteo$date >= bridge$first_usable, , drop = FALSE]
  if (!nrow(meteo)) {
    stop("Open-Meteo returned no daily data after NASA POWER's last complete date.",
         call. = FALSE)
  }
  combined <- rbind(
    nasa_observed[, c("date", output_cols)],
    archive_bridge[, c("date", output_cols)],
    meteo[, c("date", output_cols)]
  )
  combined <- combined[combined$date >= weather_start_date, ]
  combined <- combined[order(combined$date), , drop = FALSE]
  if (anyDuplicated(combined$date)) {
    stop("NASA POWER/Open-Meteo merge produced duplicate dates.",
         call. = FALSE)
  }
  list(
    raw = combined, nasa_last_available_date = nasa_last_available,
    open_meteo_archive_start = if (nrow(archive_bridge)) min(archive_bridge$date) else as.Date(NA),
    open_meteo_archive_end = if (nrow(archive_bridge)) max(archive_bridge$date) else as.Date(NA),
    meteo_response_start_date = min(meteo$date),
    forecast_start_date = as.Date(reference_date),
    forecast_end_date = max(combined$date, na.rm = TRUE)
  )
}
