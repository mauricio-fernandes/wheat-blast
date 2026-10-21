# Charts for the Shiny dashboard.
#
# Design rules applied here (from the data-viz guidance):
#  * one axis per panel: no secondary axes; different quantities get their own
#    stacked panel with a shared date axis
#  * categorical hues in fixed order (blue, orange, aqua: validated all-pairs);
#    a single series uses blue and needs no legend, the panel title names it
#  * multi-series panels carry a legend AND direct end labels (the aqua slot is
#    below 3:1 contrast, so labels and the data tables are the relief)
#  * thin marks, solid hairline grid, dashed lines only for thresholds/projection
#  * the forecast period is a light band plus a dashed start line
# Needs ggplot2 and gridExtra attached (R/00_packages.R does this).

viz <- list(
  surface = "#fcfcfb", ink = "#0b0b0b", ink2 = "#52514e", grid = "#e6e5e1",
  blue = "#2a78d6", orange = "#eb6834", aqua = "#1baf7a", neutral = "#b8b7b1"
)

theme_viz <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = viz$ink2),
      plot.background = ggplot2::element_rect(fill = viz$surface, colour = NA),
      panel.background = ggplot2::element_rect(fill = viz$surface, colour = NA),
      panel.grid.major = ggplot2::element_line(colour = viz$grid, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = viz$ink2, size = base_size - 2),
      plot.title = ggplot2::element_text(
        colour = viz$ink, face = "bold", size = base_size, hjust = 0,
        margin = ggplot2::margin(b = 4)
      ),
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = viz$ink2, size = base_size - 1),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),
      plot.margin = ggplot2::margin(6, 70, 6, 6)
    )
}

# Stack panels vertically with aligned plot areas. Falls back to plain stacking
# if the gtable widths cannot be aligned.
stack_plots <- function(plots, heights = NULL) {
  grobs <- lapply(plots, ggplot2::ggplotGrob)
  aligned <- tryCatch({
    max_widths <- do.call(grid::unit.pmax, lapply(grobs, function(g) g$widths))
    lapply(grobs, function(g) { g$widths <- max_widths; g })
  }, error = function(e) grobs)
  invisible(gridExtra::grid.arrange(
    grobs = aligned, ncol = 1, heights = heights %||% rep(1, length(aligned))
  ))
}

# Light band for the forecast period and a dashed line where it starts.
forecast_layers <- function(start, end) {
  if (is.null(start) || length(start) != 1L || is.na(start)) return(NULL)
  list(
    ggplot2::annotate("rect", xmin = start, xmax = end, ymin = -Inf, ymax = Inf,
                      fill = viz$ink2, alpha = 0.07),
    ggplot2::geom_vline(xintercept = start, linetype = "dashed",
                        colour = viz$ink2, linewidth = 0.4)
  )
}

# Light orange band for the heading window.
band_layers <- function(range_dates) {
  if (is.null(range_dates) || anyNA(range_dates)) return(NULL)
  ggplot2::annotate("rect", xmin = range_dates[1], xmax = range_dates[2],
                    ymin = -Inf, ymax = Inf, fill = viz$orange, alpha = 0.10)
}

# Series name at the last point of each series, in text colour (not series colour).
end_labels <- function(long_df) {
  last <- do.call(rbind, lapply(split(long_df, long_df$series), function(x) {
    x[which.max(x$Date), , drop = FALSE]
  }))
  ggplot2::geom_text(
    data = last, mapping = ggplot2::aes(x = Date, y = value, label = series),
    hjust = -0.12, size = 3.2, colour = viz$ink2, show.legend = FALSE,
    inherit.aes = FALSE
  )
}

multi_line_panel <- function(base, long_df, colors, markers = FALSE) {
  p <- base +
    ggplot2::geom_line(
      data = long_df, ggplot2::aes(y = value, colour = series), linewidth = 0.8
    )
  if (markers) {
    p <- p + ggplot2::geom_point(
      data = long_df, ggplot2::aes(y = value, fill = series), shape = 21,
      colour = viz$surface, stroke = 0.8, size = 2.4, show.legend = FALSE
    ) + ggplot2::scale_fill_manual(values = colors, guide = "none")
  }
  p +
    ggplot2::scale_colour_manual(values = colors, breaks = names(colors)) +
    end_labels(long_df)
}

date_range_of <- function(dates) range(as.Date(dates), na.rm = TRUE)

# --- Forecast outlook ----------------------------------------------------------

# Rain, temperature, wetness/susceptibility and daily kernel-infection gain for
# the forecast window (plus a week of context). `fc` = forecast_summary list.
plot_outlook <- function(fc, params, context_days = 7) {
  d <- fc$daily
  first_fc <- which(d$Source == "OPEN_METEO_FORECAST")[1]
  d <- d[seq.int(max(1L, first_fc - context_days), nrow(d)), , drop = FALSE]
  start <- fc$summary$Forecast_Start_Date
  xr <- date_range_of(d$Date) + c(-0.5, 0.5)
  fl <- forecast_layers(start, xr[2])
  base <- function(title) {
    ggplot2::ggplot(d, ggplot2::aes(Date)) +
      ggplot2::labs(title = title) +
      ggplot2::scale_x_date(date_labels = "%d %b") +
      ggplot2::coord_cartesian(xlim = xr, clip = "off") +
      theme_viz()
  }

  p_rain <- base("Rain (mm/day)") + fl +
    ggplot2::geom_col(ggplot2::aes(y = Rain_mm), fill = viz$blue, width = 0.7)

  long_t <- rbind(
    data.frame(Date = d$Date, value = d$Tmax, series = "Tmax"),
    data.frame(Date = d$Date, value = d$Tmin, series = "Tmin")
  )
  p_temp <- multi_line_panel(
    base("Daily temperature (°C)") + fl, long_t,
    c(Tmax = viz$orange, Tmin = viz$blue), markers = TRUE
  )

  min_hours <- params$infection_min_wet_hours
  p_hours <- base("Hours per day with high relative humidity") + fl +
    ggplot2::geom_col(ggplot2::aes(y = RH_Wet_Hours), fill = viz$blue, width = 0.7) +
    ggplot2::geom_hline(yintercept = min_hours, linetype = "dashed",
                        colour = viz$ink2, linewidth = 0.4) +
    ggplot2::annotate("text", x = xr[1], y = min_hours,
                      label = "minimum wet period for infection", hjust = 0,
                      vjust = -0.6, size = 3, colour = viz$ink2) +
    ggplot2::scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, 6))

  long_w <- rbind(
    data.frame(Date = d$Date, value = d$Infection_Temp_Factor,
               series = "Temperature response"),
    data.frame(Date = d$Date, value = d$Heading_Susceptibility,
               series = "Heading susceptibility")
  )
  p_wet <- multi_line_panel(
    base("Temperature response and spike susceptibility (0–1)") + fl, long_w,
    c(`Temperature response` = viz$orange, `Heading susceptibility` = viz$aqua),
    markers = TRUE
  ) +
    ggplot2::scale_y_continuous(limits = c(0, 1))

  gain <- d[is.finite(d$Kernel_Infection_Change), , drop = FALSE]
  gain$status <- factor(
    ifelse(gain$Infection_Conditions_Met, "Infection conditions met",
           "Conditions not met"),
    levels = c("Infection conditions met", "Conditions not met")
  )
  p_gain <- ggplot2::ggplot(
    gain, ggplot2::aes(Date, Kernel_Infection_Change, fill = status)
  ) + fl +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(
      values = c(`Infection conditions met` = viz$blue,
                 `Conditions not met` = viz$neutral),
      drop = FALSE
    ) +
    ggplot2::labs(title = "Kernel infection gain per day (relative units)") +
    ggplot2::scale_x_date(date_labels = "%d %b") +
    ggplot2::coord_cartesian(xlim = xr, clip = "off") +
    theme_viz()

  stack_plots(list(p_rain, p_temp, p_hours, p_wet, p_gain),
              heights = c(1, 1.15, 1.15, 1.15, 1.15))
}

# --- Season plots --------------------------------------------------------------

line_panel <- function(df, column, title, x_scale, layers = NULL, log10 = FALSE) {
  df <- data.frame(Date = df$Date, value = df[[column]])
  p <- ggplot2::ggplot(df, ggplot2::aes(Date, value)) + layers +
    ggplot2::geom_line(colour = viz$blue, linewidth = 0.8) +
    ggplot2::labs(title = title) + x_scale + theme_viz()
  if (log10) p <- p + ggplot2::scale_y_log10()
  p
}

season_layers <- function(output, forecast_start) {
  hw <- NULL
  if ("Heading_Susceptibility" %in% names(output)) {
    idx <- which(output$Heading_Susceptibility > 0)
    if (length(idx)) hw <- as.Date(range(output$Date[idx]))
  }
  list(
    band_layers(hw),
    forecast_layers(forecast_start, as.Date(max(output$Date)))
  )
}

season_x_scale <- function(dates) {
  ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %d",
                        limits = date_range_of(dates))
}

forecast_start_of <- function(res) {
  fs <- res$forecast_summary
  if (is.null(fs)) NULL else fs$summary$Forecast_Start_Date
}

plot_disease <- function(res) {
  out <- res$output
  out$Date <- as.Date(out$Date)
  x_scale <- season_x_scale(out$Date)
  layers <- season_layers(out, forecast_start_of(res))
  min_hours <- res$params$infection_min_wet_hours
  wet_hours <- line_panel(out, "RH_Wet_Hours",
                          "Hours per day with high relative humidity",
                          x_scale, layers) +
    ggplot2::geom_hline(yintercept = min_hours, linetype = "dashed",
                        colour = viz$ink2, linewidth = 0.4) +
    ggplot2::annotate("text", x = min(out$Date), y = min_hours,
                      label = "minimum wet period for infection", hjust = 0,
                      vjust = -0.6, size = 3, colour = viz$ink2) +
    ggplot2::scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, 6))
  temp_response <- line_panel(out, "Infection_Temp_Factor",
                              "Temperature response for infection (0–1)",
                              x_scale, layers)
  stack_plots(list(
    line_panel(out, "Blast_Kernel_Infection", "Kernel infection (relative units)",
               x_scale, layers),
    line_panel(out, "Blast_Kernel_Spores", "Spores on kernels (relative units)",
               x_scale, layers),
    line_panel(out, "Blast_Airborne_Spores",
               "Airborne spores (relative units, log scale)", x_scale, layers,
               log10 = TRUE),
    line_panel(out, "Leaf_Disease_Severity_Pct",
               "Leaf lesion severity (% of leaf area)", x_scale, layers),
    wet_hours, temp_response,
    line_panel(out, "Blast_Sporulation_Viability",
               "Sporulation viability after cold (0–1)", x_scale, layers)
  ))
}

plot_crop <- function(res) {
  out <- res$output
  out$Date <- as.Date(out$Date)
  x_scale <- season_x_scale(out$Date)
  layers <- season_layers(out, forecast_start_of(res))

  pheno <- res$phenology
  pheno$Date <- as.Date(pheno$Date)
  p_tt <- line_panel(out, "Thermal_Time", "Thermal time (°C day)", x_scale,
                     layers) +
    ggplot2::geom_vline(data = pheno, ggplot2::aes(xintercept = Date),
                        colour = viz$ink2, linewidth = 0.3) +
    ggplot2::geom_text(
      data = pheno, ggplot2::aes(x = Date, y = 0, label = Stage),
      angle = 90, hjust = 0, vjust = -0.5, size = 3, colour = viz$ink2,
      inherit.aes = FALSE
    )

  soil <- res$soil_water
  soil$Date <- as.Date(res$weather$date[soil$tday])
  fc_theta <- res$params$topsoil_fc
  wp_theta <- res$params$topsoil_wp
  p_soil <- line_panel(soil, "topsoil_theta",
                       "Topsoil water content (m³/m³)", x_scale, layers) +
    ggplot2::geom_hline(yintercept = c(fc_theta, wp_theta), linetype = "dashed",
                        colour = viz$ink2, linewidth = 0.4) +
    ggplot2::annotate("text", x = min(soil$Date), y = c(fc_theta, wp_theta),
                      label = c("field capacity", "wilting point"), hjust = 0,
                      vjust = -0.6, size = 3, colour = viz$ink2)
  p_root <- line_panel(soil, "root_zone_swc",
                       "Root-zone water (fraction of field capacity)", x_scale,
                       layers)

  stack_plots(list(
    p_tt,
    line_panel(out, "LAI", "Leaf area index", x_scale, layers),
    line_panel(out, "Biomass", "Biomass (g/m²)", x_scale, layers),
    p_soil, p_root
  ))
}

plot_weather <- function(res) {
  w <- res$weather
  w$Date <- as.Date(w$date)
  x_scale <- season_x_scale(w$Date)
  src <- if ("weather_source" %in% names(w)) w$weather_source else character()
  fstart <- if (any(src == "OPEN_METEO_FORECAST")) {
    min(w$Date[src == "OPEN_METEO_FORECAST"])
  } else NULL
  layers <- forecast_layers(fstart, max(w$Date))
  base <- function(title) {
    ggplot2::ggplot(w, ggplot2::aes(Date)) + layers +
      ggplot2::labs(title = title) + x_scale + theme_viz()
  }

  p_rain <- base("Rain (mm/day)") +
    ggplot2::geom_col(ggplot2::aes(y = rain), fill = viz$blue, width = 1)
  long_t <- rbind(
    data.frame(Date = w$Date, value = w$tmin, series = "Tmin"),
    data.frame(Date = w$Date, value = w$tmax, series = "Tmax"),
    data.frame(Date = w$Date, value = w$dpt, series = "Dew point")
  )
  p_temp <- multi_line_panel(
    base("Temperature and dew point (°C)"), long_t,
    c(Tmin = viz$blue, Tmax = viz$orange, `Dew point` = viz$aqua)
  )
  p_srad <- base("Solar radiation (MJ/m²/day)") +
    ggplot2::geom_line(ggplot2::aes(y = srad), colour = viz$blue, linewidth = 0.6)
  min_hours <- res$params$infection_min_wet_hours
  p_wet <- base("Hours per day with high relative humidity") +
    ggplot2::geom_line(ggplot2::aes(y = rh_wet_hours), colour = viz$blue,
                       linewidth = 0.6) +
    ggplot2::geom_hline(yintercept = min_hours, linetype = "dashed",
                        colour = viz$ink2, linewidth = 0.4) +
    ggplot2::annotate("text", x = min(w$Date), y = min_hours,
                      label = "minimum wet period for infection", hjust = 0,
                      vjust = -0.6, size = 3, colour = viz$ink2) +
    ggplot2::scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, 6))

  stack_plots(list(p_rain, p_temp, p_srad, p_wet), heights = c(1, 1.3, 1, 1.1))
}

# Single-panel fallback for the overview when there are no forecast days.
plot_kernel_progress <- function(res) {
  out <- res$output
  out$Date <- as.Date(out$Date)
  x_scale <- season_x_scale(out$Date)
  line_panel(out, "Blast_Kernel_Infection", "Kernel infection (relative units)",
             x_scale, season_layers(out, forecast_start_of(res)))
}
