# ============================================================================
# 10_simulation.R
# run_simulation(): ties weather, soil, ODE, phenology and summaries together.
# ============================================================================

# --- 8. Run simulation ------------------------------------------------------

run_simulation <- function(config = simulation_config,
                           params = model_params,
                           soil_layers = soil_layers_df,
                           initial_state = NULL) {
  if (is.null(config$leaf_initial_inoculum_date))
    config$leaf_initial_inoculum_date <- NA
  if (is.null(config$leaf_initial_inoculum_size))
    config$leaf_initial_inoculum_size <- 0
  if (is.null(config$blast_initial_residue_inoculum))
    config$blast_initial_residue_inoculum <- 100
  if (is.null(config$spore_dynamics_lead_days))
    config$spore_dynamics_lead_days <- 60
  if (config$spore_dynamics_lead_days < 0)
    stop("spore_dynamics_lead_days must be zero or positive.",
         call. = FALSE)
  simulation_start_date <- config$start_date -
    config$spore_dynamics_lead_days
  crop_sowing_day <- as.numeric(config$start_date - simulation_start_date) + 1
  if (crop_sowing_day < 1) {
    stop("Crop sowing date must be on or after weather_start_date.",
         call. = FALSE)
  }
  if (is.null(initial_state)) {
    initial_state <- default_initial_state(
      config$blast_initial_residue_inoculum,
      params$blast_airborne_spore_minimum,
      params$alternative_host_initial_infectious_area
    )
  }

  params$topsoil_wp <- soil_layers$WP[1]
  params$topsoil_fc <- soil_layers$FC[1]
  weather_source <- toupper(config$weather_source %||% "NASA_POWER_OPEN_METEO")
  nasa_last_available_date <- as.Date(NA)
  if (weather_source == "NASA_POWER_OPEN_METEO") {
    blended <- fetch_blended_weather(
      latitude = config$latitude,
      longitude = config$longitude,
      weather_start_date = simulation_start_date,
      reference_date = config$reference_date %||% Sys.Date(),
      nasa_community = config$nasa_community %||% "AG",
      nasa_time_standard = config$nasa_time_standard %||% "LST",
      open_meteo_timezone = config$open_meteo_timezone %||% "auto",
      open_meteo_past_days = config$open_meteo_past_days %||% 92,
      open_meteo_forecast_days = config$open_meteo_forecast_days %||% 16,
      rh_threshold = config$rh_wet_threshold_pct %||% 90
    )
    nasa_last_available_date <- blended$nasa_last_available_date
    weather_data <- preprocess_weather_data(
      blended$raw, simulation_start_date, blended$forecast_end_date
    )
    archive_start_date <- blended$open_meteo_archive_start
    archive_end_date <- blended$open_meteo_archive_end
    forecast_start_date <- blended$forecast_start_date
    meteo_response_start_date <- blended$meteo_response_start_date
    weather_data$weather_source <- ifelse(
      weather_data$date <= nasa_last_available_date, "NASA_POWER",
      ifelse(weather_data$date < meteo_response_start_date,
             "OPEN_METEO_ARCHIVE",
             ifelse(weather_data$date < forecast_start_date,
                    "OPEN_METEO_RECENT", "OPEN_METEO_FORECAST"))
    )
  } else if (weather_source == "NASA_POWER") {
    raw_weather <- fetch_nasa_power_hourly(
      latitude = config$latitude, longitude = config$longitude,
      start_date = simulation_start_date,
      end_date = config$end_date %||% Sys.Date(),
      community = config$nasa_community %||% "AG",
      time_standard = config$nasa_time_standard %||% "LST",
      rh_threshold = config$rh_wet_threshold_pct %||% 90
    )
    weather_cols <- c("tmax", "tmin", "dpt", "rain", "srad")
    complete_rows <- complete.cases(raw_weather[, weather_cols]) &
      apply(raw_weather[, weather_cols, drop = FALSE], 1,
            function(row) all(is.finite(row) & row > -900))
    if (!any(complete_rows)) {
      stop("NASA POWER returned no complete daily weather rows.",
           call. = FALSE)
    }
    last_complete_date <- max(raw_weather$date[complete_rows])
    trailing_partial <- raw_weather$date > last_complete_date
    if (any(trailing_partial)) {
      message(
        "NASA-only mode: stopping at the last fully observed date, ",
        format(last_complete_date), "; excluding ", sum(trailing_partial),
        " later partial day(s) through ",
        format(max(raw_weather$date[trailing_partial])),
        ". Select NASA POWER + Open-Meteo for current-season extension."
      )
      raw_weather <- raw_weather[!trailing_partial, , drop = FALSE]
    }
    nasa_last_available_date <- last_complete_date
    weather_data <- preprocess_weather_data(
      raw_weather, simulation_start_date,
      last_complete_date
    )
    weather_data$weather_source <- "NASA_POWER"
  } else if (weather_source == "CSV") {
    weather_data <- load_and_preprocess_weather(
      config$weather_file_path, simulation_start_date,
      config$end_date %||% Sys.Date()
    )
    weather_data$weather_source <- "CSV"
  } else {
    stop("weather_source must be 'NASA_POWER_OPEN_METEO', 'NASA_POWER', or 'CSV'.",
         call. = FALSE)
  }
  simulation_end_date <- min(
    max(weather_data$date), config$end_date %||% max(weather_data$date)
  )
  weather_data <- weather_data[weather_data$date <= simulation_end_date, ]
  if (min(weather_data$date) > simulation_start_date) {
    stop(
      "Weather data must begin by ", simulation_start_date,
      " to simulate ", config$spore_dynamics_lead_days,
      " pre-sowing days.", call. = FALSE
    )
  }
  message("Infection rule: favorable temperature (", params$leaf_Tmin, "-",
          params$leaf_Tmax, " C, optimum ", params$leaf_Topt, " C) and at least ",
          params$infection_min_wet_hours, " h/day with RH >= ",
          config$rh_wet_threshold_pct %||% 90, " %.")
  soil_water_data <- simulate_soil_water_balance(
    weather_data, soil_layers, params, crop_sowing_day
  )
  output_df <- run_crop_disease_ode(
    weather_data, soil_water_data, params, simulation_start_date,
    config$start_date,
    config$time_step, initial_state,
    config$leaf_initial_inoculum_date,
    config$leaf_initial_inoculum_size
  )
  maturity_reached <- FALSE
  maturity_row <- which(output_df$Thermal_Time >= params$TTM)
  if (isTRUE(config$stop_at_maturity) && length(maturity_row) > 0) {
    # Retain output only through the first model time step at physiological
    # maturity, then trim forcing and soil-water results to the same date.
    output_df <- output_df[seq_len(maturity_row[1]), , drop = FALSE]
    simulation_end_date <- as.Date(tail(output_df$Date, 1))
    weather_data <- weather_data[weather_data$date <= simulation_end_date, ,
                                 drop = FALSE]
    soil_water_data <- soil_water_data[seq_len(nrow(weather_data)), ,
                                       drop = FALSE]
    maturity_reached <- TRUE
  } else if (isTRUE(config$stop_at_maturity)) {
    warning(
      "TTM (", params$TTM, ") was not reached within the available weather/forecast window; ",
      "simulation ends at the last available weather date (",
      format(simulation_end_date), ").",
      call. = FALSE
    )
  }
  pheno_data <- calculate_phenological_stages(
    output_df, params, simulation_start_date
  )
  # Tag every output row with the weather source that drove it (day index).
  output_df$Weather_Source <- weather_data$weather_source[
    pmin(nrow(weather_data), pmax(1L, floor(output_df$Time + 1e-9)))
  ]
  forecast_summary <- summarize_forecast_window(
    output_df, weather_data, params
  )
  forecast_start_marker <- if (!is.null(forecast_summary)) {
    forecast_summary$summary$Forecast_Start_Date
  } else NULL
  imputation_flag_names <- intersect(
    paste0("imputed_", c("tmax", "tmin", "dpt", "rain", "srad")),
    names(weather_data)
  )
  imputed_value_count <- if (length(imputation_flag_names)) {
    sum(as.matrix(weather_data[imputation_flag_names]), na.rm = TRUE)
  } else 0L
  imputed_dates <- if ("weather_imputed" %in% names(weather_data)) {
    weather_data$date[weather_data$weather_imputed]
  } else as.Date(character())
  imputed_variable_names <- if (length(imputation_flag_names)) {
    sub("^imputed_", "", imputation_flag_names[
      vapply(weather_data[imputation_flag_names], any, logical(1), na.rm = TRUE)
    ])
  } else character()
  wheat_blast_summary <- data.frame(
    Spore_Dynamics_Start_Date = simulation_start_date,
    Simulation_End_Date = simulation_end_date,
    Weather_Mode = config$weather_source,
    Imputed_Weather_Day_Count = length(imputed_dates),
    Imputed_Weather_Value_Count = imputed_value_count,
    Imputed_Weather_Dates = paste(format(imputed_dates), collapse = ","),
    Imputed_Weather_Variables = paste(imputed_variable_names, collapse = ","),
    Stop_At_Maturity = isTRUE(config$stop_at_maturity),
    Maturity_Reached = maturity_reached,
    TTM_Target = params$TTM,
    Final_Thermal_Time = tail(output_df$Thermal_Time, 1),
    NASA_Last_Available_Date = nasa_last_available_date,
    OpenMeteo_Archive_Start = if (exists("archive_start_date")) archive_start_date else as.Date(NA),
    OpenMeteo_Archive_End = if (exists("archive_end_date")) archive_end_date else as.Date(NA),
    OpenMeteo_Recent_Start = if (exists("meteo_response_start_date")) meteo_response_start_date else as.Date(NA),
    OpenMeteo_Forecast_Start = if (exists("forecast_start_date")) forecast_start_date else as.Date(NA),
    Crop_Sowing_Date = config$start_date,
    Pre_Sowing_Days = config$spore_dynamics_lead_days,
    Initial_Residue_Inoculum =
      config$blast_initial_residue_inoculum,
    Minimum_Sporulation_Viability =
      min(output_df$Blast_Sporulation_Viability, na.rm = TRUE),
    Final_Sporulation_Viability =
      tail(output_df$Blast_Sporulation_Viability, 1),
    Alternative_Host_Enabled = params$alternative_host_enabled,
    Alternative_Host_LAI = params$alternative_host_lai,
    Alternative_Host_Initial_Infectious_Area =
      params$alternative_host_initial_infectious_area,
    Alternative_Host_Spore_Production_Multiplier =
      params$alternative_host_spore_production_multiplier,
    Initial_Inoculum_Date = config$leaf_initial_inoculum_date,
    Initial_Inoculum_Size = config$leaf_initial_inoculum_size,
    Final_Severity_Pct = tail(output_df$Leaf_Disease_Severity_Pct, 1),
    Max_Severity_Pct = max(output_df$Leaf_Disease_Severity_Pct,
                           na.rm = TRUE),
    Final_AUDPC = max(output_df$Leaf_Disease_AUDPC, na.rm = TRUE),
    Max_Leaf_Spores = max(output_df$Leaf_Spores, na.rm = TRUE),
    Max_Alternative_Host_Severity_Pct =
      max(output_df$Alternative_Host_Severity_Pct, na.rm = TRUE),
    Final_Alternative_Host_AUDPC =
      max(output_df$Alternative_Host_AUDPC, na.rm = TRUE),
    Max_Alternative_Host_Spores =
      max(output_df$Alternative_Host_Spores, na.rm = TRUE),
    Max_Airborne_Spores =
      max(output_df$Blast_Airborne_Spores, na.rm = TRUE),
    Min_Airborne_Spores =
      min(output_df$Blast_Airborne_Spores, na.rm = TRUE),
    Max_Kernel_Spores =
      max(output_df$Blast_Kernel_Spores, na.rm = TRUE),
    Final_Kernel_Infection =
      tail(output_df$Blast_Kernel_Infection, 1),
    Max_Kernel_Infection =
      max(output_df$Blast_Kernel_Infection, na.rm = TRUE),
    Max_Leaf_Lesion_Coverage_Pct =
      max(output_df$Leaf_Lesion_Coverage_Pct, na.rm = TRUE),
    Max_Heading_Susceptibility =
      max(output_df$Heading_Susceptibility, na.rm = TRUE),
    Mean_RH_Wet_Hours = mean(weather_data$rh_wet_hours, na.rm = TRUE),
    Days_Wet_Period_Met = sum(
      weather_data$rh_wet_hours >= params$infection_min_wet_hours, na.rm = TRUE
    ),
    Forecast_Days_Simulated = if (is.null(forecast_summary)) 0L else
      forecast_summary$summary$Forecast_Days,
    Forecast_Kernel_Infection_Gain = if (is.null(forecast_summary)) NA_real_ else
      forecast_summary$summary$Kernel_Infection_Gain
  )
  print_forecast_summary(forecast_summary)
  if (isTRUE(config$make_plots)) {
    plot_simulation_results(output_df, soil_water_data,
                            pheno_data, soil_layers,
                            params = params,
                            forecast_start = forecast_start_marker)
    print(plot_weather_data(weather_data))
  }
  list(
    config = config, params = params, weather = weather_data,
    soil_water = soil_water_data, output = output_df,
    phenology = pheno_data,
    wheat_blast_summary = wheat_blast_summary,
    forecast_summary = forecast_summary
  )
}
