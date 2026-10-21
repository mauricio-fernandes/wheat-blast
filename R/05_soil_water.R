# ============================================================================
# 05_soil_water.R
# Daily soil water balance.
# ============================================================================

# --- 3. Soil water balance --------------------------------------------------

simulate_soil_water_balance <- function(weather_data_filtered,
                                        initial_soil_layers,
                                        model_params,
                                        crop_sowing_day = 1) {
  n_days <- nrow(weather_data_filtered)
  n_layers <- nrow(initial_soil_layers)
  soil_layers <- initial_soil_layers
  swc_daily <- matrix(NA_real_, n_days, n_layers)
  colnames(swc_daily) <- paste0("L", seq_len(n_layers))
  root_zone_swc_normalized <- rep(NA_real_, n_days)
  daily_tt <- pmax(0, weather_data_filtered$tmean - model_params$Tbase)
  daily_tt[seq_len(min(n_days, max(0, crop_sowing_day - 1)))] <- 0
  cumulative_tt <- cumsum(daily_tt)
  layer_bottoms <- cumsum(soil_layers$thickness)

  for (day in seq_len(n_days)) {
    rain <- weather_data_filtered$rain[day]
    eto <- weather_data_filtered$eto[day]
    root_depth <- calculate_root_depth(
      cumulative_tt[day], model_params$TT_Emergence,
      model_params$TT_FlagLeaf, model_params$initial_root_depth,
      model_params$max_root_depth
    )

    runoff <- max(0, rain - model_params$runoff_threshold)
    infil_left <- rain - runoff
    for (i in seq_len(n_layers)) {
      theta <- mm_to_m3m3(soil_layers$SWC[i], soil_layers$thickness[i])
      max_add <- max(0, (soil_layers$SAT[i] - theta) *
                       soil_layers$thickness[i] * 1000)
      add <- min(infil_left, max_add)
      soil_layers$SWC[i] <- soil_layers$SWC[i] + add
      infil_left <- infil_left - add
      if (infil_left <= 0) break
    }

    top_wp_mm <- m3m3_to_mm(soil_layers$WP[1], soil_layers$thickness[1])
    evap_available <- max(0, soil_layers$SWC[1] - top_wp_mm)
    evap <- min(eto * model_params$evap_fraction, evap_available)
    soil_layers$SWC[1] <- soil_layers$SWC[1] - evap

    transp_max <- if (day >= crop_sowing_day) {
      eto * model_params$transp_fraction
    } else 0
    root_idx <- which(layer_bottoms <= root_depth + 1e-9)
    if (length(root_idx) == 0) root_idx <- 1L
    available <- soil_layers$SWC[root_idx] -
      m3m3_to_mm(soil_layers$WP[root_idx],
                 soil_layers$thickness[root_idx])
    available <- pmax(0, available)
    total_available <- sum(available)
    if (total_available > 0) {
      actual_transp <- min(transp_max, total_available)
      uptake <- actual_transp * available / total_available
      soil_layers$SWC[root_idx] <- soil_layers$SWC[root_idx] - uptake
    }

    if (n_layers > 1) {
      for (i in seq_len(n_layers - 1)) {
        theta <- mm_to_m3m3(soil_layers$SWC[i],
                            soil_layers$thickness[i])
        drain <- max(0, theta - soil_layers$FC[i]) *
          soil_layers$thickness[i] * 1000
        soil_layers$SWC[i] <- soil_layers$SWC[i] - drain
        soil_layers$SWC[i + 1] <- soil_layers$SWC[i + 1] + drain
      }
    }
    theta_n <- mm_to_m3m3(soil_layers$SWC[n_layers],
                          soil_layers$thickness[n_layers])
    bottom_drain <- max(0, theta_n - soil_layers$FC[n_layers]) *
      soil_layers$thickness[n_layers] * 1000
    soil_layers$SWC[n_layers] <-
      soil_layers$SWC[n_layers] - bottom_drain

    swc_daily[day, ] <- soil_layers$SWC
    root_water <- sum(soil_layers$SWC[root_idx])
    root_capacity <- sum(soil_layers$FC[root_idx] *
                         soil_layers$thickness[root_idx] * 1000)
    root_zone_swc_normalized[day] <-
      if (root_capacity > 0) root_water / root_capacity else 0
  }

  data.frame(
    tday = seq_len(n_days),
    topsoil_theta = swc_daily[, 1] /
      (initial_soil_layers$thickness[1] * 1000),
    p = weather_data_filtered$rain,
    srad = weather_data_filtered$srad,
    tmean = weather_data_filtered$tmean,
    tmax = weather_data_filtered$tmax,
    tmin = weather_data_filtered$tmin,
    dpt = weather_data_filtered$dpt,
    rh_wet_hours = weather_data_filtered$rh_wet_hours,
    root_zone_swc = root_zone_swc_normalized
  )
}
