# App helpers need yaml (parse_overrides, settings_to_yaml); load them only when present.
if (requireNamespace("yaml", quietly = TRUE)) {
  source(file.path(project_root, "app", "helpers.R"))
}

valid_inputs <- function(...) {
  v <- list(
    location_name = "Passo Fundo", latitude = -28.262, longitude = -52.406,
    timezone = "America/Sao_Paulo",
    sowing_date = as.Date("2026-06-20"), cultivar = "Test", susceptibility = 0.8,
    TT_FlagLeaf = 1030, TT_Anthesis = 1300, TT_GrainFill = 2200, TTM = 2350,
    weather_mode = "nasa", hindcast = FALSE, hindcast_date = NULL,
    forecast_days = 16, nasa_community = "AG", nasa_time_standard = "LST",
    csv_path = NULL, lead_days = 60, residue = 100,
    stop_at_maturity = TRUE, time_step = 0.1,
    alt_enabled = TRUE, alt_lai = 10, alt_area = 0.1, alt_mult = 2,
    rh_threshold = 90, min_wet_hours = 8,
    inf_tmin = 15, inf_topt = 28, inf_tmax = 30,
    spike_start = 1100, spike_peak = 1300, spike_end = 1500,
    kernel_start = 1400, hard_dough = 2200,
    leaf_max_frac = 0.8, leaf_lesion = 0.005, leaf_crowding = 2,
    soil_FC = "0.30, 0.30, 0.30, 0.30", soil_WP = "0.15, 0.15, 0.15, 0.15",
    soil_SAT = "0.45, 0.45, 0.45, 0.45", overrides = ""
  )
  modifyList(v, list(...))
}
