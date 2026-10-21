# WHEAT BLAST MODEL: HOURLY-WEATHER INFECTION RULE
# Forecast mode: hourly NASA POWER data up to its last complete day, then hourly
# Open-Meteo data (recent days and the 16-day forecast), summarised to daily values.
# Default coordinates are Passo Fundo, RS, Brazil; edit latitude/longitude.
# Optional CSV source requires: date, rain, tmax, tmin, dpt, srad, rh_wet_hours.
# Temperatures are degrees Celsius; radiation is MJ m-2 day-1; rain is mm/day;
# rh_wet_hours is the number of hours per day with relative humidity at or above
# the wet threshold (weather.rh_wet_threshold_pct, default 90 %).
#
# Infection rule: leaf, alternative-host and spike infection need BOTH a favorable
# temperature (cardinal response on the daily mean temperature) AND a wet period of
# at least pathogen.infection_min_wet_hours (default 8) hours per day. There is no
# canopy-moisture index and no dew-point response any more.
#
# Acknowledgment: This simple model simulates soil water balance, wheat growth
# and development, and wheat blast. It was adapted from the original FHB and
# generic foliar-disease teaching model developed
# by J. M. Fernandes for participants in the international training course
# "Wheat Diseases and Resistance Breeding", CIMMYT, Mexico, August 3-22, 2025.

# --- Locate this script's folder (modules live in ./R) ------------------------
command_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command_args, value = TRUE)
sourced_file <- NULL   # set when the script is run with source("path/run_forecast.R")
for (frame in rev(sys.frames())) {
  if (!is.null(frame$ofile)) { sourced_file <- frame$ofile; break }
}
script_directory <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else if (!is.null(sourced_file)) {
  dirname(normalizePath(sourced_file))
} else {
  getwd()
}

# --- Load modules in numeric order -------------------------------------------
module_files <- sort(list.files(
  file.path(script_directory, "R"), pattern = "^[0-9]{2}_.*\\.R$",
  full.names = TRUE
))
if (!length(module_files)) {
  stop("No model modules found in ", file.path(script_directory, "R"), call. = FALSE)
}
for (module_file in module_files) source(module_file)
check_required_packages()

# --- Choose the configuration -------------------------------------------------
# Rscript run_forecast.R                      base wheat_blast_model_config.yaml
# Rscript run_forecast.R passo-fundo_2026-06-20   one site config (exact name)
# Rscript run_forecast.R passo-fundo          latest sowing date <= today
# Rscript run_forecast.R --all                every file in configs/ (no plots)
# Rscript run_forecast.R --list               list the available configs
# Add --report to also write an HTML report (needs rmarkdown + pandoc), and
# --lang=pt for a Portuguese report (default: report.language in the YAML).
script_args <- commandArgs(trailingOnly = TRUE)
base_config_path <- file.path(script_directory, "wheat_blast_model_config.yaml")
if (!file.exists(base_config_path)) {
  working_directory_config <- file.path(getwd(), "wheat_blast_model_config.yaml")
  if (file.exists(working_directory_config)) {
    base_config_path <- working_directory_config
  } else {
    stop("Configuration file not found. Expected: ", base_config_path,
         " (or ", working_directory_config, ").", call. = FALSE)
  }
}
configs_dir <- file.path(dirname(base_config_path), "configs")
locations_path <- file.path(dirname(base_config_path), "locations.yaml")
cultivars_path <- file.path(dirname(base_config_path), "cultivars.yaml")
outputs_dir <- file.path(dirname(base_config_path), "outputs")

save_run_outputs <- function(results, name) {
  folder <- file.path(outputs_dir, name)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  write.csv(results$output, file.path(folder, "simulation_results.csv"), row.names = FALSE)
  write.csv(results$phenology, file.path(folder, "phenology_results.csv"), row.names = FALSE)
  if (!is.null(results$forecast_summary)) {
    write.csv(results$forecast_summary$summary, file.path(folder, "forecast_summary.csv"), row.names = FALSE)
    write.csv(results$forecast_summary$daily, file.path(folder, "forecast_daily.csv"), row.names = FALSE)
  }
  message("Outputs written to ", folder)
}

do_report <- "--report" %in% script_args
lang_arg <- sub("^--lang=", "", grep("^--lang=", script_args, value = TRUE))

make_report <- function(results, loaded, name) {
  file <- file.path(outputs_dir, name, paste0("report_", name, ".html"))
  render_simulation_report(
    results, file,
    language = if (length(lang_arg)) lang_arg[1] else loaded$config$report_language %||% "en",
    author = loaded$config$report_author, recipients = loaded$config$report_recipients,
    cultivar = loaded$cultivar, susceptibility = loaded$cultivar_susceptibility,
    soil_layers = loaded$soil
  )
  message("Report written to ", file)
}

if ("--list" %in% script_args) {
  print(list_site_configs(configs_dir)[, c("name", "location", "sowing_date")], row.names = FALSE)
  quit(save = "no", status = 0)
}

if ("--all" %in% script_args) {
  table <- list_site_configs(configs_dir)
  if (!nrow(table)) stop("No site configurations in ", configs_dir, call. = FALSE)
  collected <- list(); failed <- character()
  for (i in seq_len(nrow(table))) {
    name <- table$name[i]
    message("\n=== ", name, " (", i, "/", nrow(table), ") ===")
    ok <- tryCatch({
      loaded <- load_site_configuration(table$file[i], base_config_path, locations_path, cultivars_path)
      loaded$config$make_plots <- FALSE
      res <- run_simulation(config = loaded$config, params = loaded$params,
                            soil_layers = loaded$soil)
      save_run_outputs(res, name)
      if (do_report) tryCatch(make_report(res, loaded, name),
                              error = function(e) message("Report failed: ", conditionMessage(e)))
      if (!is.null(res$forecast_summary)) {
        collected[[name]] <- cbind(Config = name, Location = table$location[i],
                                   Sowing_Date = table$sowing_date[i],
                                   res$forecast_summary$summary)
      }
      TRUE
    }, error = function(e) { message("FAILED: ", conditionMessage(e)); FALSE })
    if (!ok) failed <- c(failed, name)
  }
  if (length(collected)) {
    combined <- do.call(rbind, collected)
    dir.create(outputs_dir, showWarnings = FALSE)
    out_file <- file.path(outputs_dir, paste0("all_forecast_summaries_", format(Sys.Date()), ".csv"))
    write.csv(combined, out_file, row.names = FALSE)
    message("\nCombined summary: ", out_file)
    print(combined[, c("Config", "Phenology_At_Start", "Days_Infection_Conditions_Met",
                       "Kernel_Infection_Gain", "Signal")], row.names = FALSE)
  }
  if (length(failed)) message("\nFailed: ", paste(failed, collapse = ", "))
  quit(save = "no", status = if (length(failed)) 1L else 0L)
}

selector <- script_args[!startsWith(script_args, "--")]
run_name <- NULL
if (length(selector)) {
  chosen <- resolve_site_config(selector[1], configs_dir)
  yaml_config_path <- chosen$file
  run_name <- chosen$name
  yaml_settings <- load_site_configuration(chosen$file, base_config_path, locations_path, cultivars_path)
} else {
  yaml_config_path <- base_config_path
  yaml_settings <- load_yaml_configuration(base_config_path)
}
simulation_config <- yaml_settings$config
model_params <- yaml_settings$params
soil_layers_df <- yaml_settings$soil
message("Loaded YAML configuration: ", normalizePath(yaml_config_path))
message("Location: ", simulation_config$location_name)
message("Cultivar: ", yaml_settings$cultivar,
        "; susceptibility: ", yaml_settings$cultivar_susceptibility)
message("Sowing date: ", format(simulation_config$start_date),
        "; model start date: ", format(
          simulation_config$start_date -
            simulation_config$spore_dynamics_lead_days
        ))
simulation_results <- run_simulation()
if (!is.null(run_name)) save_run_outputs(simulation_results, run_name)
if (do_report) {
  report_name <- run_name %||% paste0(slugify_location(simulation_config$location_name), "_",
                                      format(simulation_config$start_date))
  tryCatch(make_report(simulation_results, yaml_settings, report_name),
           error = function(e) message("Report failed: ", conditionMessage(e)))
}

# Original object names retained for teaching notes and downstream code.
preprocessed_weather <- simulation_results$weather
soil_water_data_sim <- simulation_results$soil_water
output_df_sim <- simulation_results$output
pheno_data_sim <- simulation_results$phenology

forecast_summary_sim <- simulation_results$forecast_summary

# Optional exports:
# write.csv(output_df_sim, "simulation_results.csv", row.names = FALSE)
# write.csv(pheno_data_sim, "phenology_results.csv", row.names = FALSE)
# if (!is.null(forecast_summary_sim)) {
#   write.csv(forecast_summary_sim$summary, "forecast_summary.csv", row.names = FALSE)
#   write.csv(forecast_summary_sim$daily, "forecast_daily.csv", row.names = FALSE)
# }
