# ============================================================================
# 03_response_functions.R
# Pure response functions: wet-period gate, root depth, water stress,
# cardinal temperature, heading susceptibility, AUDPC.
# ============================================================================

# Infection needs a wet period: at least `min_hours` hours per day with relative
# humidity at or above the wet threshold (rh_wet_hours, from hourly weather).
# Returns 1 when the condition is met, 0 when not, NA when hours are unknown.
# Combined with a favorable temperature in the ODE (cardinal response).
calculate_wet_period_gate <- function(hours, min_hours = 8) {
  if (!is.finite(min_hours) || min_hours < 0 || min_hours > 24) {
    stop("infection_min_wet_hours must be between 0 and 24.", call. = FALSE)
  }
  ifelse(is.na(hours), NA_real_, as.numeric(hours >= min_hours))
}

calculate_root_depth <- function(thermal_time, tt_emergence, tt_flagleaf,
                                 initial_depth = 0.1, max_depth = 1.5) {
  if (thermal_time < tt_emergence) return(initial_depth)
  if (thermal_time >= tt_flagleaf) return(max_depth)
  initial_depth + (max_depth - initial_depth) *
    (thermal_time - tt_emergence) / (tt_flagleaf - tt_emergence)
}

water_stress_function <- function(normalized_swc,
                                  fc_threshold = 0.7,
                                  wp_threshold = 0.3) {
  if (normalized_swc >= fc_threshold) return(1)
  if (normalized_swc <= wp_threshold) return(0)
  clamp01((normalized_swc - wp_threshold) /
            (fc_threshold - wp_threshold))
}

cardinal_temperature_response <- function(temp, tmin, topt, tmax) {
  if (is.na(temp) || temp <= tmin || temp >= tmax ||
      tmin >= topt || topt >= tmax) return(0)
  if (temp <= topt) return((temp - tmin) / (topt - tmin))
  (tmax - temp) / (tmax - topt)
}

# Triangular heading susceptibility: low before heading, maximal near
# anthesis, and declining afterwards.
calculate_heading_susceptibility <- function(tt, start, peak, end) {
  if (!is.finite(tt) || start >= peak || peak >= end) return(0)
  if (tt <= start || tt >= end) return(0)
  if (tt <= peak) return((tt - start) / (peak - start))
  (end - tt) / (end - peak)
}

calculate_audpc <- function(time, severity_percent) {
  if (length(time) < 2) return(rep(0, length(time)))
  intervals <- diff(time) *
    (head(severity_percent, -1) + tail(severity_percent, -1)) / 2
  c(0, cumsum(intervals))
}
