# ============================================================================
# 09_forecast.R
# Forecast-window summary and printing.
# ============================================================================

# --- 7b. Forecast-window summary --------------------------------------------

phenology_stage_at <- function(thermal_time, params) {
  stages <- c("Pre-emergence", "Emergence", "DoubleRidge", "TerminalSpikelet",
              "FlagLeaf", "Anthesis", "InitialKernelFormation", "HardDough",
              "PhysiologicalMaturity")
  thresholds <- c(-Inf, params$TT_Emergence, params$TT_DoubleRidge,
                  params$TT_TerminalSpikelet, params$TT_FlagLeaf,
                  params$TT_Anthesis, params$blast_kernel_start_TT,
                  params$blast_hard_dough_TT, params$TTM)
  if (!is.finite(thermal_time)) return(NA_character_)
  stages[max(which(thermal_time >= thresholds))]
}

# Summarizes the days driven by the Open-Meteo forecast (weather_source ==
# "OPEN_METEO_FORECAST"; the first forecast row is the reference date/today).
#
# State columns are values at the START of each calendar day (t = 1, 2, ...);
# *_Change columns are next-day start minus this-day start. run_crop_disease_ode()
# integrates up to the start of the last weather day, so the last day's own
# weather is not yet reflected and its change is NA.
#
# Kernel infection is in relative model units (unbounded), so the summary
# reports the gain over the window and compares it with the largest daily gain
# before the forecast instead of assigning arbitrary risk classes.
summarize_forecast_window <- function(output_df, weather_data, params) {
  if (!"weather_source" %in% names(weather_data)) return(NULL)
  fc_days <- which(weather_data$weather_source == "OPEN_METEO_FORECAST")
  if (!length(fc_days)) return(NULL)
  n_days <- nrow(weather_data)
  day_rows <- vapply(seq_len(n_days), function(d) {
    idx <- which(abs(output_df$Time - d) < 1e-6)
    if (length(idx)) idx[1] else NA_integer_
  }, integer(1))
  if (anyNA(day_rows[fc_days])) return(NULL)

  daily <- data.frame(
    Date = weather_data$date,
    Source = weather_data$weather_source,
    Rain_mm = weather_data$rain,
    Tmin = weather_data$tmin,
    Tmean = weather_data$tmean,
    Tmax = weather_data$tmax,
    # Hours per day with RH at or above the wet threshold, and the temperature
    # response for infection (0-1, from the daily mean temperature).
    RH_Wet_Hours = weather_data$rh_wet_hours,
    Wet_Period_Met = weather_data$rh_wet_hours >= params$infection_min_wet_hours,
    Infection_Temp_Factor = output_df$Infection_Temp_Factor[day_rows],
    Thermal_Time = output_df$Thermal_Time[day_rows],
    Heading_Susceptibility = output_df$Heading_Susceptibility[day_rows],
    Airborne_Spores = output_df$Blast_Airborne_Spores[day_rows],
    Kernel_Spores = output_df$Blast_Kernel_Spores[day_rows],
    Kernel_Infection = output_df$Blast_Kernel_Infection[day_rows],
    Leaf_Severity_Pct = output_df$Leaf_Disease_Severity_Pct[day_rows]
  )
  daily$Kernel_Infection_Change <-
    c(daily$Kernel_Infection[-1], NA_real_) - daily$Kernel_Infection
  # Conditions the model itself requires for spike infection: spike inside the
  # heading window, a favorable temperature and a wet period of at least
  # infection_min_wet_hours hours of RH at or above the wet threshold.
  daily$Infection_Conditions_Met <-
    daily$Heading_Susceptibility > 0 &
    daily$Infection_Temp_Factor > 0 &
    daily$Wet_Period_Met

  win <- daily[fc_days, , drop = FALSE]
  first_fc <- fc_days[1]
  before <- daily[seq_len(first_fc - 1), , drop = FALSE]
  pre_max_gain <- if (nrow(before) &&
                      any(is.finite(before$Kernel_Infection_Change))) {
    max(before$Kernel_Infection_Change, na.rm = TRUE)
  } else NA_real_
  gain_values <- win$Kernel_Infection_Change[is.finite(win$Kernel_Infection_Change)]
  peak_idx <- if (length(gain_values) && max(gain_values) > 0) {
    which.max(replace(win$Kernel_Infection_Change,
                      !is.finite(win$Kernel_Infection_Change), -Inf))
  } else NA_integer_
  days_susceptible <- sum(win$Heading_Susceptibility > 0, na.rm = TRUE)
  days_conditions <- sum(win$Infection_Conditions_Met, na.rm = TRUE)
  days_wet <- sum(win$Wet_Period_Met, na.rm = TRUE)
  days_temp <- sum(win$Infection_Temp_Factor > 0, na.rm = TRUE)
  signal <- if (days_susceptible == 0) {
    "Spikes outside the heading window during the forecast: no new spike infection expected"
  } else if (days_conditions == 0) {
    sprintf(paste0("Spikes in the heading window, but no forecast day combines a ",
                   "favorable temperature with at least %g wet hours"),
            params$infection_min_wet_hours)
  } else {
    sprintf("Spikes in the heading window and infection conditions met on %d of %d forecast day(s)",
            days_conditions, nrow(win))
  }
  inf_start <- win$Kernel_Infection[1]
  inf_end <- tail(win$Kernel_Infection, 1)

  summary_df <- data.frame(
    Forecast_Start_Date = win$Date[1],
    Forecast_End_Date = tail(win$Date, 1),
    Forecast_Days = nrow(win),
    Phenology_At_Start = phenology_stage_at(win$Thermal_Time[1], params),
    Phenology_At_End = phenology_stage_at(tail(win$Thermal_Time, 1), params),
    Thermal_Time_At_Start = win$Thermal_Time[1],
    Thermal_Time_At_End = tail(win$Thermal_Time, 1),
    Forecast_Rain_Total_mm = sum(win$Rain_mm, na.rm = TRUE),
    Forecast_Rain_Days_ge_1mm = sum(win$Rain_mm >= 1, na.rm = TRUE),
    Forecast_Mean_Tmean_C = mean(win$Tmean, na.rm = TRUE),
    Forecast_Mean_RH_Wet_Hours = if (any(is.finite(win$RH_Wet_Hours))) {
      mean(win$RH_Wet_Hours, na.rm = TRUE)
    } else NA_real_,
    Days_Wet_Period_Met = days_wet,
    Days_Favorable_Temperature = days_temp,
    Days_In_Heading_Window = days_susceptible,
    Max_Heading_Susceptibility = max(win$Heading_Susceptibility, na.rm = TRUE),
    Days_Infection_Conditions_Met = days_conditions,
    Airborne_Spores_At_Start = win$Airborne_Spores[1],
    Airborne_Spores_At_End = tail(win$Airborne_Spores, 1),
    Airborne_Spores_Max = max(win$Airborne_Spores, na.rm = TRUE),
    Kernel_Infection_At_Start = inf_start,
    Kernel_Infection_At_End = inf_end,
    Kernel_Infection_Gain = inf_end - inf_start,
    Kernel_Infection_Peak_Gain_Date = if (is.na(peak_idx)) as.Date(NA) else win$Date[peak_idx],
    Kernel_Infection_Peak_Daily_Gain = if (is.na(peak_idx)) 0 else win$Kernel_Infection_Change[peak_idx],
    Pre_Forecast_Max_Daily_Gain = pre_max_gain,
    Leaf_Severity_Pct_At_End = tail(win$Leaf_Severity_Pct, 1),
    Signal = signal,
    stringsAsFactors = FALSE
  )
  list(summary = summary_df, daily = daily, window = win)
}

print_forecast_summary <- function(forecast) {
  if (is.null(forecast)) {
    message("Forecast summary: no Open-Meteo forecast days in the simulated ",
            "period (NASA-only/CSV mode, hindcast, or maturity reached first).")
    return(invisible(NULL))
  }
  s <- forecast$summary
  format_value <- function(x) {
    if (inherits(x, "Date")) format(x) else
      paste(format(x, digits = 4), collapse = ",")
  }
  cat("\n=== Forecast-window summary (Open-Meteo forecast days) ===\n")
  print(data.frame(
    Metric = names(s),
    Value = vapply(s, format_value, character(1)),
    row.names = NULL
  ), right = FALSE)
  cat("Note: kernel infection is in relative model units; the last weather ",
      "day is not yet integrated.\n", sep = "")
  invisible(forecast)
}
