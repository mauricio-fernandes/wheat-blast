# ============================================================================
# 02_params.R
# Default configuration, soil layers, model parameters and the YAML loader.
# ============================================================================

# --- 1. Configuration and setup ---------------------------------------------

forecast_year <- as.integer(format(Sys.Date(), "%Y"))
simulation_config <- list(
  # start_date is crop sowing date; adjust to the local field calendar.
  start_date = as.Date(sprintf("%d-06-20", forecast_year)),
  # Calculated from sowing date minus spore_dynamics_lead_days.
  # NULL means derive the weather/simulation start from the crop calendar.
  weather_start_date = NULL,
  end_date = NULL,
  nasa_power_only = TRUE,
  nasa_power_open_meteo = FALSE,
  latitude = -28.262,
  longitude = -52.406,
  nasa_community = "AG",
  nasa_time_standard = "LST",
  # Hourly NASA POWER / Open-Meteo data are summarised to days, including the
  # hours per day with relative humidity >= rh_wet_threshold_pct (wet period).
  rh_wet_threshold_pct = 90,
  open_meteo_timezone = "America/Sao_Paulo",
  open_meteo_past_days = 92,
  open_meteo_forecast_days = 16,
  weather_file_path = file.path(
    path.expand("~/Documents/CIMMYT_TRaining_2026"),
    "weather_passoFundo.csv"
  ),
  time_step = 0.1,
  make_plots = TRUE,
  spore_dynamics_lead_days = 60,
  leaf_initial_inoculum_date = NA,
  leaf_initial_inoculum_size = 0,
  blast_initial_residue_inoculum = 100
)

soil_layers_df <- data.frame(
  layer = 1:4,
  depth = c(0.1, 0.2, 0.4, 0.8),
  thickness = c(0.1, 0.1, 0.2, 0.4),
  FC = c(0.35, 0.33, 0.31, 0.27),
  WP = c(0.10, 0.10, 0.09, 0.08),
  SAT = c(0.40, 0.38, 0.36, 0.32)
)
soil_layers_df$SWC <- soil_layers_df$FC * soil_layers_df$thickness * 1000

model_params <- list(
  # Crop model
  ALPHA = 0.20,
  LAIMAX = 5.0,
  RUE = 2.0,
  K = 0.46,
  Tbase = 0.0,
  LAI_SenescenceRate = 0.08,

  # Phenology (degree-days)
  TT_Emergence = 90.0,
  TT_DoubleRidge = 530,
  TT_TerminalSpikelet = 700,
  TT_FlagLeaf = 1030,
  TT_Anthesis = 1300,
  TT_Watery = 1400,
  TT_GrainFill = 2200,
  TTM = 2350,

  # Wheat blast: residue inoculum and airborne spores
  blast_residue_spore_rate = 0.08,
  blast_residue_moisture_threshold = 0.35,
  blast_release_Tmin = 15,
  blast_release_Topt = 28,
  blast_release_Tmax = 30,
  # Cold injury has a carry-over effect on fungal structures. Injury begins
  # near freezing and is maximal at or below the full-damage temperature.
  blast_cold_damage_threshold_C = 3,
  blast_cold_full_damage_C = -1,
  blast_cold_damage_rate = 0.5,
  blast_cold_recovery_rate = 0.01,
  # Rainfall response: rain up to 5 mm promotes spore release, whereas
  # excess rainfall suppresses release and washes spores from the air.
  blast_rain_optimum_mm = 5,
  blast_light_rain_benefit_max = 0.50,
  blast_heavy_rain_release_suppression_per_mm = 0.5,
  blast_air_washout_rate_per_mm = 0.20,
  blast_airborne_mortality_rate = 0.20,
  # Persistent background inoculum: airborne spores cannot fall below this
  # value, allowing recovery after cold, washout, or other severe losses.
  blast_airborne_spore_minimum = 10,
  blast_leaf_deposition_rate = 0.1,
  blast_spike_deposition_rate = 0.01,
  blast_foliar_airborne_fraction = 0.60,

  # Wheat blast infection of kernels
  blast_kernel_infection_rate = 0.01,
  blast_kernel_spore_mortality_rate = 0.12,
  blast_kernel_start_TT = 1400,  # initial kernel formation
  blast_hard_dough_TT = 2200,
  # Spike infection is emphasized around heading/anthesis rather than
  # being equally susceptible throughout the entire grain-filling period.
  blast_spike_heading_start_TT = 1150,
  blast_spike_heading_peak_TT = 1300,
  blast_spike_heading_end_TT = 1550,
  blast_spike_susceptibility = 1.0,
  blast_kernel_recovery_rate = 0,

  # Foliar disease model
  leaf_disease_enabled = TRUE,
  leaf_Tmin = 15,
  leaf_Topt = 28,
  leaf_Tmax = 30,
  leaf_infection_rate = 0.2,
  leaf_max_lesion_size = 0.002, # effective diseased area per infection, LAI units
  leaf_max_diseased_fraction = 0.15, # foliar lesion-bearing area cap
  leaf_infection_crowding_exponent = 2.0,
  leaf_latent_period = 10,
  leaf_infectious_period = 21,
  leaf_primary_inoculum_duration = 5,
  leaf_spore_production_rate = 15000,
  leaf_spore_mortality_rate = 0.18,
  leaf_spore_infection_efficiency = 0.2,

  # Alternative host. Its fixed leaf area is present throughout the
  # simulation, including the pre-sowing period. Infection and sporulation
  # use the same biological parameters as wheat foliage.
  alternative_host_enabled = TRUE,
  alternative_host_lai = 10.0,
  # Initial infectious area provides an external inoculum source when the
  # alternative host is already diseased at the start of simulation.
  alternative_host_initial_infectious_area = 0.10,
  alternative_host_spore_production_multiplier = 2.0,

  # Infection (leaf, alternative host and spike) needs BOTH a favorable
  # temperature (cardinal response leaf_Tmin / leaf_Topt / leaf_Tmax, applied to
  # the daily mean temperature) AND a wet period: at least this many hours per
  # day with relative humidity >= weather.rh_wet_threshold_pct.
  infection_min_wet_hours = 8,

  # Filled from the top soil layer by run_simulation()
  topsoil_wp = NA_real_,
  topsoil_fc = NA_real_,

  # Water balance
  runoff_threshold = 12,
  evap_fraction = 0.4,
  transp_fraction = 0.6,
  initial_root_depth = 0.1,
  max_root_depth = 1.5
)

load_yaml_configuration <- function(path, base_config = simulation_config,
                                    base_params = model_params,
                                    base_soil = soil_layers_df) {
  if (!file.exists(path)) stop("YAML configuration not found: ", path, call. = FALSE)
  cfg <- yaml::read_yaml(path)
  config <- base_config; params <- base_params; soil <- base_soil
  location <- cfg$location %||% list(); crop <- cfg$crop %||% list()
  weather <- cfg$weather %||% list(); simulation <- cfg$simulation %||% list()
  host <- cfg$alternative_host %||% list(); pathogen <- cfg$pathogen %||% list()
  overrides <- cfg$overrides %||% list()
  config$latitude <- as.numeric(location$latitude %||% config$latitude)
  config$longitude <- as.numeric(location$longitude %||% config$longitude)
  config$location_name <- location$name %||% "unspecified"
  config$open_meteo_timezone <- location$timezone %||% config$open_meteo_timezone
  config$start_date <- as.Date(crop$sowing_date %||% config$start_date)
  config$nasa_power_only <- isTRUE(weather$nasa_power_only)
  config$nasa_power_open_meteo <- isTRUE(weather$nasa_power_open_meteo)
  if (sum(c(config$nasa_power_only,
            config$nasa_power_open_meteo)) != 1L) {
    stop(
      "Set exactly one weather option to true: ",
      "weather.nasa_power_only or weather.nasa_power_open_meteo.",
      call. = FALSE
    )
  }
  config$weather_source <- if (config$nasa_power_only) {
    "NASA_POWER"
  } else {
    "NASA_POWER_OPEN_METEO"
  }
  # Weather starts the configured number of lead days before sowing.
  # Ignore the legacy year_to_date_start field if present in an older YAML.
  config$weather_start_date <- NULL
  config$reference_date <- if (is.null(weather$reference_date)) {
    sowing_year <- as.integer(format(config$start_date, "%Y"))
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    if (sowing_year < current_year) {
      as.Date(sprintf("%d-12-31", sowing_year))
    } else {
      NULL
    }
  } else as.Date(weather$reference_date)
  # Keep a historical run inside its configured crop year. For the current
  # year, NULL allows the model to extend through the available forecast.
  if (!is.null(config$reference_date)) {
    config$end_date <- config$reference_date
  }
  config$nasa_community <- weather$nasa_community %||% config$nasa_community
  config$nasa_time_standard <- weather$nasa_time_standard %||% config$nasa_time_standard
  config$rh_wet_threshold_pct <- as.numeric(weather$rh_wet_threshold_pct %||% config$rh_wet_threshold_pct)
  if (!is.finite(config$rh_wet_threshold_pct) || config$rh_wet_threshold_pct <= 0 ||
      config$rh_wet_threshold_pct > 100) {
    stop("weather.rh_wet_threshold_pct must be between 0 and 100.", call. = FALSE)
  }
  # Report settings (language en/pt, author, recipients, risk class limits for kernel infection).
  report <- cfg$report %||% list()
  config$report_language <- if (identical(tolower(report$language %||% "en"), "pt")) "pt" else "en"
  config$report_author <- report$author
  config$report_recipients <- as.character(unlist(report$recipients))
  config$report_block_days <- as.integer(report$forecast_block_days %||% 21L)
  if (!is.finite(config$report_block_days) || config$report_block_days < 1L || config$report_block_days > 60L) {
    stop("report.forecast_block_days must be a whole number between 1 and 60.", call. = FALSE)
  }
  config$risk_thresholds <- if (is.null(report$risk_thresholds)) NULL else
    validate_risk_thresholds(unlist(report$risk_thresholds))
  config$open_meteo_past_days <- as.integer(weather$open_meteo_past_days %||% config$open_meteo_past_days)
  config$open_meteo_forecast_days <- as.integer(weather$open_meteo_forecast_days %||% config$open_meteo_forecast_days)
  if (!is.null(weather$csv_file)) config$weather_file_path <- weather$csv_file
  config$time_step <- as.numeric(simulation$time_step %||% config$time_step)
  config$make_plots <- isTRUE(simulation$make_plots %||% config$make_plots)
  config$stop_at_maturity <- isTRUE(simulation$stop_at_maturity %||% TRUE)
  config$spore_dynamics_lead_days <- as.numeric(simulation$spore_dynamics_lead_days %||% config$spore_dynamics_lead_days)
  config$blast_initial_residue_inoculum <- as.numeric(simulation$blast_initial_residue_inoculum %||% config$blast_initial_residue_inoculum)
  config$leaf_initial_inoculum_size <- as.numeric(simulation$leaf_initial_inoculum_size %||% config$leaf_initial_inoculum_size)
  if (!is.null(simulation$leaf_initial_inoculum_date)) config$leaf_initial_inoculum_date <- as.Date(simulation$leaf_initial_inoculum_date)
  params$alternative_host_enabled <- isTRUE(host$enabled %||% params$alternative_host_enabled)
  params$alternative_host_lai <- as.numeric(host$lai %||% params$alternative_host_lai)
  params$alternative_host_initial_infectious_area <- as.numeric(host$initial_infectious_area %||% params$alternative_host_initial_infectious_area)
  params$alternative_host_spore_production_multiplier <- as.numeric(host$spore_production_multiplier %||% params$alternative_host_spore_production_multiplier)
  # The thermal-time parameters sit under `crop:` in the YAML but were only read
  # from `pathogen:`, so editing them under `crop:` had no effect. Read them from
  # `crop:` first; a value in `pathogen:` (legacy layout) still takes precedence.
  for (nm in c("TT_Anthesis", "TT_FlagLeaf", "TT_GrainFill", "TTM")) {
    if (!is.null(crop[[nm]])) params[[nm]] <- as.numeric(crop[[nm]])
  }
  fields <- c(TT_Anthesis="TT_Anthesis", TT_FlagLeaf="TT_FlagLeaf", TT_GrainFill="TT_GrainFill", TTM="TTM", blast_spike_heading_start_TT="spike_heading_start_TT", blast_spike_heading_peak_TT="spike_heading_peak_TT", blast_spike_heading_end_TT="spike_heading_end_TT", blast_kernel_start_TT="kernel_start_TT", blast_hard_dough_TT="hard_dough_TT", leaf_max_diseased_fraction="leaf_max_diseased_fraction", leaf_max_lesion_size="leaf_max_lesion_size", leaf_infection_crowding_exponent="leaf_infection_crowding_exponent", infection_min_wet_hours="infection_min_wet_hours", leaf_Tmin="infection_Tmin_C", leaf_Topt="infection_Topt_C", leaf_Tmax="infection_Tmax_C")
  for (nm in names(fields)) {
    value <- pathogen[[fields[[nm]]]]
    if (is.null(value)) next
    params[[nm]] <- as.numeric(value)
  }
  if (!is.finite(params$infection_min_wet_hours) ||
      params$infection_min_wet_hours < 0 || params$infection_min_wet_hours > 24) {
    stop("pathogen.infection_min_wet_hours must be between 0 and 24.", call. = FALSE)
  }
  if (!(params$leaf_Tmin < params$leaf_Topt && params$leaf_Topt < params$leaf_Tmax)) {
    stop("pathogen.infection_Tmin_C < infection_Topt_C < infection_Tmax_C is required.",
         call. = FALSE)
  }
  # Keys of the removed dew / canopy-moisture wetness model are ignored; say so.
  obsolete_keys <- intersect(names(pathogen), c(
    "dew_enabled", "dew_direct_multiplier", "dew_spread_onset_C",
    "dew_spread_end_C", "leaf_wetness_method", "wetness_hours_onset",
    "wetness_hours_full", "spike_moisture_threshold"
  ))
  if (length(obsolete_keys) || !is.null(weather$hourly)) {
    warning("Ignoring obsolete setting(s) of the removed dew/canopy-moisture ",
            "wetness model: ",
            paste(c(obsolete_keys, if (!is.null(weather$hourly)) "weather.hourly"),
                  collapse = ", "),
            ". Infection now needs a favorable temperature and at least ",
            "pathogen.infection_min_wet_hours hours/day with RH at or above ",
            "weather.rh_wet_threshold_pct.", call. = FALSE)
  }
  susceptibility <- as.numeric(crop$cultivar_susceptibility %||% 1)
  if (!is.finite(susceptibility) || susceptibility < 0 || susceptibility > 1) stop("crop.cultivar_susceptibility must be between 0 and 1.", call. = FALSE)
  params$blast_spike_susceptibility <- params$blast_spike_susceptibility * susceptibility
  params$leaf_infection_rate <- params$leaf_infection_rate * (0.5 + 0.5 * susceptibility)
  params$leaf_spore_production_rate <- params$leaf_spore_production_rate * (0.5 + 0.5 * susceptibility)
  for (nm in names(overrides)) {
    if (nm %in% names(params)) params[[nm]] <- overrides[[nm]] else if (nm %in% names(config)) config[[nm]] <- overrides[[nm]] else warning("Ignoring unknown YAML override: ", nm, call. = FALSE)
  }
  if (!is.null(cfg$soil$FC)) soil$FC <- as.numeric(unlist(cfg$soil$FC))
  if (!is.null(cfg$soil$WP)) soil$WP <- as.numeric(unlist(cfg$soil$WP))
  if (!is.null(cfg$soil$SAT)) soil$SAT <- as.numeric(unlist(cfg$soil$SAT))
  soil$SWC <- soil$FC * soil$thickness * 1000
  list(config=config, params=params, soil=soil, cultivar=crop$cultivar %||% "unspecified", cultivar_susceptibility=susceptibility)
}
