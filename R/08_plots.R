# ============================================================================
# 08_plots.R
# Plotting.
# ============================================================================

# --- 7. Plotting ------------------------------------------------------------

plot_simulation_results <- function(output_df, soil_water_data, pheno_data,
                                    soil_layers = soil_layers_df,
                                    params = model_params,
                                    forecast_start = NULL) {
  # Dashed marker where the Open-Meteo forecast begins (NULL = no marker).
  forecast_marker <- if (!is.null(forecast_start) && !is.na(forecast_start)) {
    geom_vline(xintercept = as.numeric(as.Date(forecast_start)),
               linetype = "dashed", color = "firebrick", linewidth = 0.7)
  } else NULL
  p_soil_water <- ggplot(
    soil_water_data, aes(tday, topsoil_theta)
  ) +
    geom_line(color = "brown", linewidth = 1) +
    geom_hline(yintercept = soil_layers$FC[1], linetype = "dashed",
               color = "steelblue") +
    geom_hline(yintercept = soil_layers$WP[1], linetype = "dotted",
               color = "firebrick") +
    labs(title = "Simulated Topsoil Water Content",
         x = "Time (days)",
         y = expression(theta ~ "(m"^3 ~ m^-3 * ")")) +
    theme_minimal()

  p_tt <- ggplot(output_df, aes(Date, Thermal_Time)) +
    geom_line(color = "darkblue", linewidth = 1) +
    geom_vline(data = pheno_data, aes(xintercept = Date),
               linetype = "dashed", color = "darkgreen") +
    geom_text(
      data = pheno_data,
      aes(x = Date, y = max(output_df$Thermal_Time) * 0.40,
          label = paste0(Stage, " (", round(TT_Threshold), " C d)")),
      hjust = -0.1, angle = 90, size = 3, color = "darkgreen"
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
    labs(title = "Thermal Time and Phenology",
         x = "Date", y = "Thermal time (degree-days)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  biomass_max <- max(output_df$Biomass, na.rm = TRUE)
  lai_max <- max(output_df$LAI, na.rm = TRUE)
  biomass_scale <- if (biomass_max > 0) lai_max / biomass_max else 1
  p_lai_biomass <- ggplot(output_df, aes(Date)) +
    geom_line(aes(y = LAI, color = "LAI"), linewidth = 1) +
    geom_line(aes(y = Biomass * biomass_scale,
                  color = "Biomass"), linewidth = 1) +
    scale_y_continuous(
      "Leaf area index",
      sec.axis = sec_axis(~ . / biomass_scale,
                          name = "Biomass (g m-2)")
    ) +
    scale_color_manual(values = c(LAI = "forestgreen",
                                  Biomass = "darkorange")) +
    labs(title = "Simulated LAI and Biomass", x = "Date", color = "") +
    theme_minimal()

  p_spores <- ggplot(output_df, aes(Date)) +
    geom_line(aes(y = Blast_Airborne_Spores,
                  color = "Airborne spores"),
              linewidth = 1) +
    geom_line(aes(y = Blast_Kernel_Spores,
                  color = "Spores on kernels"),
              linewidth = 1) +
    forecast_marker +
    scale_color_manual(values = c("Airborne spores" = "purple",
                                  "Spores on kernels" = "darkred")) +
    labs(title = "Simulated Wheat Blast Spore Dynamics",
         x = "Date", y = "Relative units", color = "") +
    theme_minimal()

  p_infection <- ggplot(
    output_df, aes(Date, Blast_Kernel_Infection)
  ) +
    geom_line(color = "black", linewidth = 1) +
    forecast_marker +
    labs(title = "Simulated Wheat Blast Kernel Infection",
         x = "Date", y = "Relative units") +
    theme_minimal()

  # The two infection conditions: hours per day with RH at or above the wet
  # threshold (line, dotted minimum) and the temperature response (0-1).
  max_hours <- 24
  p_infection_conditions <- ggplot(output_df, aes(Date)) +
    geom_line(aes(y = RH_Wet_Hours, color = "Hours with high RH"), linewidth = 0.8) +
    geom_line(aes(y = Infection_Temp_Factor * max_hours,
                  color = "Temperature response"), linewidth = 0.8) +
    geom_hline(yintercept = params$infection_min_wet_hours, linetype = "dotted",
               color = "firebrick") +
    forecast_marker +
    scale_color_manual(values = c(
      "Hours with high RH" = "darkblue", "Temperature response" = "darkorange"
    )) +
    scale_y_continuous(
      "Hours per day with RH at or above the wet threshold",
      limits = c(0, max_hours),
      sec.axis = sec_axis(~ . / max_hours, name = "Temperature response (0-1)")
    ) +
    labs(title = "Infection Conditions: Wet Period and Temperature",
         subtitle = sprintf("Infection needs at least %g wet hours per day and a favorable temperature",
                            params$infection_min_wet_hours),
         x = "Date", color = "") +
    theme_minimal()

  p_cold_viability <- ggplot(
    output_df, aes(Date, Blast_Sporulation_Viability)
  ) +
    geom_line(color = "steelblue4", linewidth = 1) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      title = "Fungal Sporulation Viability After Cold Exposure",
      x = "Date", y = "Viability (0-1)"
    ) +
    theme_minimal()

  p_leaf_disease <- ggplot(output_df, aes(Date)) +
    geom_line(aes(y = Leaf_Healthy, color = "Healthy"), linewidth = 1) +
    geom_line(aes(y = Leaf_Latent, color = "Latent"), linewidth = 1) +
    geom_line(aes(y = Leaf_Infectious, color = "Infectious"),
              linewidth = 1) +
    geom_line(aes(y = Leaf_Necrotic, color = "Necrotic"),
              linewidth = 1) +
    scale_color_manual(values = c(
      Healthy = "forestgreen", Latent = "goldenrod",
      Infectious = "firebrick", Necrotic = "gray30"
    )) +
    labs(title = "Simulated Wheat Blast Foliar Lesion Dynamics",
         x = "Date", y = "Leaf area index units", color = "") +
    theme_minimal()

  p_alternative_host <- ggplot(output_df, aes(Date)) +
    geom_line(
      aes(y = Alternative_Host_Healthy, color = "Healthy"),
      linewidth = 1
    ) +
    geom_line(
      aes(y = Alternative_Host_Latent, color = "Latent"),
      linewidth = 1
    ) +
    geom_line(
      aes(y = Alternative_Host_Infectious, color = "Infectious"),
      linewidth = 1
    ) +
    geom_line(
      aes(y = Alternative_Host_Necrotic, color = "Necrotic"),
      linewidth = 1
    ) +
    scale_color_manual(values = c(
      Healthy = "darkgreen", Latent = "goldenrod",
      Infectious = "firebrick", Necrotic = "gray30"
    )) +
    labs(
      title = "Wheat Blast on the Alternative Host",
      x = "Date", y = "Alternative-host leaf area", color = ""
    ) +
    theme_minimal()

  final_audpc <- max(output_df$Leaf_Disease_AUDPC, na.rm = TRUE)
  p_leaf_progress <- ggplot(
    output_df, aes(Date, Leaf_Disease_Severity_Pct)
  ) +
    geom_area(fill = "firebrick", alpha = 0.18) +
    geom_line(color = "firebrick", linewidth = 1.1) +
    forecast_marker +
    scale_y_continuous(limits = c(0, 100),
                       labels = function(x) paste0(x, "%")) +
    labs(title = "Wheat Blast Foliar Disease Progress Curve",
         subtitle = paste0("Final AUDPC = ", round(final_audpc, 1),
                           " %-days"),
         x = "Date", y = "Disease severity") +
    theme_minimal()

  p_leaf_spores <- ggplot(output_df, aes(Date)) +
    geom_line(aes(y = Leaf_Spores), color = "darkred", linewidth = 1.1) +
    forecast_marker +
    labs(title = "Wheat Blast Foliar Spore Dynamics",
         x = "Date", y = "Spores") +
    theme_minimal()

  grid.arrange(p_soil_water, p_tt, p_lai_biomass, ncol = 1)
  grid.arrange(p_spores, p_infection, p_cold_viability,
               p_infection_conditions, ncol = 1)
  grid.arrange(
    p_leaf_progress, p_leaf_spores, p_leaf_disease,
    p_alternative_host, ncol = 1
  )
}

plot_weather_data <- function(weather_data) {
  weekly <- weather_data %>%
    mutate(week_start = floor_date(date, unit = "week")) %>%
    group_by(week_start) %>%
    summarize(tmax = max(tmax), tmin = min(tmin), dpt = mean(dpt),
              rain = sum(rain), .groups = "drop")
  temp_long <- melt(
    weekly, id.vars = "week_start",
    measure.vars = c("tmax", "tmin", "dpt"),
    variable.name = "temperature_type",
    value.name = "temperature"
  )
  max_rain <- max(weekly$rain, na.rm = TRUE)
  if (!is.finite(max_rain) || max_rain <= 0) max_rain <- 1
  temp_limit <- max(35, ceiling(max(temp_long$temperature, na.rm = TRUE)))
  scale_factor <- max_rain / temp_limit

  ggplot(weekly, aes(week_start)) +
    geom_col(aes(y = rain, fill = "Rain"), alpha = 0.6) +
    geom_line(
      data = temp_long,
      aes(y = temperature * scale_factor, color = temperature_type),
      linewidth = 1
    ) +
    geom_point(
      data = temp_long,
      aes(y = temperature * scale_factor, color = temperature_type),
      size = 2
    ) +
    scale_y_continuous(
      "Rain (mm)",
      sec.axis = sec_axis(~ . / scale_factor,
                          name = "Temperature (deg C)")
    ) +
    scale_color_manual(
      name = "Temperature",
      values = c(tmax = "darkred", tmin = "darkblue", dpt = "darkorange"),
      labels = c(tmax = "Maximum", tmin = "Minimum", dpt = "Dew point")
    ) +
    scale_fill_manual(name = "", values = c(Rain = "skyblue")) +
    labs(title = "Weekly Weather Data", x = "Date") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
