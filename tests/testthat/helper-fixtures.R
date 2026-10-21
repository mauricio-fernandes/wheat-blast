# Shared synthetic 10-day fixture for the forecast-summary tests.

fixture_params <- list(
  TT_Emergence = 90, TT_DoubleRidge = 530, TT_TerminalSpikelet = 700,
  TT_FlagLeaf = 1030, TT_Anthesis = 1300, blast_kernel_start_TT = 1400,
  blast_hard_dough_TT = 2200, TTM = 2350,
  infection_min_wet_hours = 8
)

default_source <- c(rep("NASA_POWER", 4), rep("OPEN_METEO_RECENT", 2),
                    rep("OPEN_METEO_FORECAST", 4))

make_fixture <- function(
    heading = c(rep(0, 6), rep(0.5, 4)),
    wet_hours = c(rep(10, 6), 5, 12, 14, 9),
    temp_factor = rep(1, 10),
    infection = c(0, 0, 0, 0, 0, 0, 0, 1, 3, 6),
    source = default_source) {
  n <- 10
  tmean <- c(rep(15, 6), 20, 20, 22, 22)
  weather <- data.frame(
    date = as.Date("2026-10-01") + 0:(n - 1),
    rain = c(rep(0, 6), 0, 2, 10, 0),
    tmean = tmean, tmax = tmean + 5, tmin = tmean - 5, dpt = tmean - 6,
    rh_wet_hours = wet_hours, weather_source = source, stringsAsFactors = FALSE
  )
  time <- seq(1, n, by = 0.1)
  day <- pmin(n, floor(time + 1e-9))
  output <- data.frame(
    Time = time,
    Thermal_Time = 100 * day + 400,
    Heading_Susceptibility = heading[day],
    Infection_Temp_Factor = temp_factor[day],
    Blast_Airborne_Spores = c(rep(10, 6), 10, 20, 40, 30)[day],
    Blast_Kernel_Spores = day,
    Blast_Kernel_Infection = infection[day],
    Leaf_Disease_Severity_Pct = day * 0.1
  )
  list(weather = weather, output = output)
}

