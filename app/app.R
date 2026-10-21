# Wheat blast risk dashboard (Shiny).
# Start it with:  Rscript run_app.R   (from the project folder), or
#                 shiny::runApp("app")
# It reuses the model modules in ../R and takes its defaults from
# ../wheat_blast_model_config.yaml.

library(shiny)

app_dir <- normalizePath(getwd())
project_root <- normalizePath(file.path(app_dir, ".."))
module_dir <- file.path(project_root, "R")
if (!dir.exists(module_dir)) {
  stop("Model modules not found in ", module_dir,
       ". Run the app from the project folder with shiny::runApp('app').",
       call. = FALSE)
}

# Load the model into the global environment (same as run_forecast.R).
module_files <- sort(list.files(
  module_dir, pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE
))
for (module_file in module_files) source(module_file)
check_required_packages()
source(file.path(app_dir, "helpers.R"))
source(file.path(app_dir, "plots_app.R"))

# --- Defaults from the YAML file --------------------------------------------------

yaml_path <- file.path(project_root, "wheat_blast_model_config.yaml")
cfg0 <- if (file.exists(yaml_path)) yaml::read_yaml(yaml_path) else list()

cfg_get <- function(path, default) {
  value <- cfg0
  for (key in path) {
    value <- if (is.list(value)) value[[key]] else NULL
    if (is.null(value)) return(default)
  }
  value
}
cfg_flag <- function(path, default) isTRUE(as.logical(cfg_get(path, default)))
cfg_vector_text <- function(path, default) {
  paste(unlist(cfg_get(path, default)), collapse = ", ")
}
pathogen_default <- function(key, param) cfg_get(c("pathogen", key), model_params[[param]])

default_sowing <- tryCatch(
  as.Date(cfg_get(c("crop", "sowing_date"), format(Sys.Date(), "%Y-06-20"))),
  error = function(e) as.Date(format(Sys.Date(), "%Y-06-20"))
)
default_mode <- if (cfg_flag(c("weather", "nasa_power_only"), FALSE)) "nasa" else "blended"

configs_dir <- file.path(project_root, "configs")
locations_path <- file.path(project_root, "locations.yaml")
cultivars_path <- file.path(project_root, "cultivars.yaml")

config_choices <- function() {
  tab <- list_site_configs(configs_dir)
  if (!nrow(tab)) return(character())
  stats::setNames(tab$name, tab$name)
}

# --- UI helpers -------------------------------------------------------------------

section <- function(title, ..., open = FALSE) {
  tags$details(class = "sect", open = if (open) NA else NULL,
               tags$summary(title), ...)
}

kpi <- function(label, value, sub = NULL) {
  div(class = "kpi",
      div(class = "label", label),
      div(class = "value", value),
      if (!is.null(sub)) div(class = "sub", sub))
}

fmt_date <- function(x) {
  x <- as.Date(x)
  if (length(x) != 1L || is.na(x)) "–" else format(x, "%d %b %Y")
}
fmt_num <- function(x, digits = 3) {
  if (length(x) != 1L || !is.finite(x)) "–" else formatC(x, digits = digits, format = "g")
}
fmt_cell <- function(x) {
  if (inherits(x, "Date")) return(fmt_date(x))
  if (is.numeric(x)) return(paste(format(round(x, 4), nsmall = 0), collapse = ", "))
  paste(as.character(x), collapse = ", ")
}
metric_table <- function(df) {
  data.frame(
    Metric = gsub("_", " ", names(df), fixed = TRUE),
    Value = vapply(df, fmt_cell, character(1)),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

headline_ui <- function(r, f) {
  sm <- r$wheat_blast_summary
  if (is.null(f)) {
    return(tagList(
      div(class = "signal",
          strong("No forecast days in this run. "),
          "Use “NASA POWER + Open-Meteo forecast” with a current sowing ",
          "date to get the forecast-window summary. Showing the whole simulated period."),
      div(class = "kpi-grid",
          kpi("Simulation end", fmt_date(sm$Simulation_End_Date)),
          kpi("Final thermal time", paste0(fmt_num(sm$Final_Thermal_Time, 4), " °C d"),
              paste("maturity at", sm$TTM_Target)),
          kpi("Peak kernel infection", fmt_num(sm$Max_Kernel_Infection), "relative units"),
          kpi("Peak leaf severity", paste0(round(sm$Max_Severity_Pct, 1), " %")),
          kpi("Weather source", sm$Weather_Mode))
    ))
  }
  s <- f$summary
  peak <- if (is.na(s$Kernel_Infection_Peak_Gain_Date)) "no gain in the window" else
    paste("largest on", fmt_date(s$Kernel_Infection_Peak_Gain_Date))
  tagList(
    div(class = "signal", strong(s$Signal)),
    div(class = "kpi-grid",
        kpi("Forecast window",
            paste(fmt_date(s$Forecast_Start_Date), "–", fmt_date(s$Forecast_End_Date)),
            paste(s$Forecast_Days, "days of Open-Meteo data")),
        kpi("Crop stage at window end", s$Phenology_At_End,
            paste0("at start: ", s$Phenology_At_Start, " (", round(s$Thermal_Time_At_Start),
                   " → ", round(s$Thermal_Time_At_End), " °C d)")),
        kpi("Rain in window", paste0(round(s$Forecast_Rain_Total_mm, 1), " mm"),
            paste(s$Forecast_Rain_Days_ge_1mm, "days with at least 1 mm")),
        kpi(paste0("Wet-period days (\u2265 ", r$params$infection_min_wet_hours, " h with RH \u2265 ",
                   r$config$rh_wet_threshold_pct %||% 90, " %)"),
            paste(s$Days_Wet_Period_Met, "of", s$Forecast_Days, "days"),
            if (is.finite(s$Forecast_Mean_RH_Wet_Hours))
              paste0("mean ", round(s$Forecast_Mean_RH_Wet_Hours, 1), " wet hours/day") else NULL),
        kpi("Days in heading window", paste(s$Days_In_Heading_Window, "of", s$Forecast_Days),
            paste("max susceptibility", round(s$Max_Heading_Susceptibility, 2))),
        kpi("Infection conditions met",
            paste(s$Days_Infection_Conditions_Met, "of", s$Forecast_Days, "days"),
            paste0("heading window, favorable temperature (", s$Days_Favorable_Temperature,
                   " days) and wet period")),
        kpi("Kernel infection gain", fmt_num(s$Kernel_Infection_Gain), peak))
  )
}

app_css <- "
.controls { background: #fcfcfb; }
details.sect { border-top: 1px solid #e6e5e1; padding: 6px 0 2px; }
details.sect > summary { font-weight: 600; cursor: pointer; margin: 4px 0 8px; color: #0b0b0b; }
.kpi-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(190px, 1fr)); gap: 12px; margin: 12px 0 18px; }
.kpi { border: 1px solid #e6e5e1; border-radius: 8px; padding: 10px 12px; background: #fcfcfb; }
.kpi .label { font-size: 12px; color: #52514e; }
.kpi .value { font-size: 22px; font-weight: 600; color: #0b0b0b; line-height: 1.25; }
.kpi .sub { font-size: 12px; color: #52514e; }
.signal { border-left: 4px solid #2a78d6; padding: 10px 14px; background: #f3f7fd; border-radius: 4px; margin: 8px 0; }
.note { color: #52514e; font-size: 13px; }
.empty { color: #52514e; padding: 40px 8px; }
table.shiny-table td, table.shiny-table th { font-size: 13px; }
"

app_theme <- if (requireNamespace("bslib", quietly = TRUE)) {
  bslib::bs_theme(version = 5, bootswatch = "flatly")
} else NULL

# --- UI ---------------------------------------------------------------------------

ui <- fluidPage(
  theme = app_theme,
  tags$head(tags$style(HTML(app_css))),
  titlePanel("Wheat blast risk dashboard"),
  sidebarLayout(
    sidebarPanel(
      width = 4, class = "controls",
      section("Saved configurations (location and sowing date)", open = TRUE,
        selectInput("saved_config", NULL, choices = config_choices()),
        fluidRow(
          column(6, actionButton("load_config", "Load into the form", class = "w-100")),
          column(6, actionButton("save_config", "Save current form", class = "w-100"))
        ),
        helpText("Files in configs/ are named <location>_<sowing date>.yaml. ",
                 "Saving writes the location, crop and soil sections to such a file ",
                 "(named from the location and sowing date below) plus the heading and kernel timings; everything else ",
                 "comes from the shared base configuration.")
      ),
      section("Location", open = TRUE,
        textInput("location_name", "Name", cfg_get(c("location", "name"), "")),
        fluidRow(
          column(6, numericInput("latitude", "Latitude",
                                 cfg_get(c("location", "latitude"), -28.262),
                                 min = -90, max = 90, step = 0.001)),
          column(6, numericInput("longitude", "Longitude",
                                 cfg_get(c("location", "longitude"), -52.406),
                                 min = -180, max = 180, step = 0.001))
        ),
        textInput("timezone", "Time zone (for Open-Meteo)",
                  cfg_get(c("location", "timezone"), "America/Sao_Paulo"))
      ),
      section("Crop", open = TRUE,
        dateInput("sowing_date", "Sowing date", value = default_sowing),
        selectInput("cultivar_pick", "Cultivar set (from cultivars.yaml)",
                    choices = c("(custom, type below)" = "", list_cultivars(cultivars_path))),
        textInput("cultivar", "Cultivar name", cfg_get(c("crop", "cultivar"), "")),
        sliderInput("susceptibility",
                    "Cultivar susceptibility (0 resistant, 1 highly susceptible)",
                    min = 0, max = 1, step = 0.05,
                    value = cfg_get(c("crop", "cultivar_susceptibility"), 1)),
        tags$details(
          tags$summary("Thermal-time thresholds (°C day)"),
          numericInput("TT_FlagLeaf", "Flag leaf",
                       cfg_get(c("crop", "TT_FlagLeaf"), model_params$TT_FlagLeaf)),
          numericInput("TT_Anthesis", "Anthesis",
                       cfg_get(c("crop", "TT_Anthesis"), model_params$TT_Anthesis)),
          numericInput("TT_GrainFill", "End of grain fill",
                       cfg_get(c("crop", "TT_GrainFill"), model_params$TT_GrainFill)),
          numericInput("TTM", "Physiological maturity",
                       cfg_get(c("crop", "TTM"), model_params$TTM))
        )
      ),
      section("Weather", open = TRUE,
        radioButtons("weather_mode", NULL, selected = default_mode, choices = c(
          "NASA POWER + Open-Meteo forecast" = "blended",
          "NASA POWER only (observed)" = "nasa",
          "CSV file" = "csv"
        )),
        conditionalPanel(
          "input.weather_mode == 'blended'",
          sliderInput("forecast_days", "Forecast days", min = 1, max = 16, step = 1,
                      value = cfg_get(c("weather", "open_meteo_forecast_days"), 16)),
          helpText("NASA POWER is used up to its last complete day; Open-Meteo ",
                   "covers the days after it, including the forecast.")
        ),
        conditionalPanel(
          "input.weather_mode == 'csv'",
          fileInput("csv_file", "Weather CSV", accept = c(".csv", "text/csv")),
          helpText("Columns: date, rain, tmax, tmin, dpt, srad and rh_wet_hours ",
                   "(hours per day with RH at or above the wet threshold).")
        ),
        conditionalPanel(
          "input.weather_mode != 'blended'",
          checkboxInput("hindcast", "Stop at a past date (hindcast)", FALSE),
          conditionalPanel(
            "input.hindcast",
            dateInput("hindcast_date", "End date", value = Sys.Date() - 1,
                      max = Sys.Date())
          )
        ),
        numericInput("rh_threshold", "Wet when relative humidity is at least (%)",
                     cfg_get(c("weather", "rh_wet_threshold_pct"), 90),
                     min = 1, max = 100, step = 1),
        helpText("Hourly NASA POWER and Open-Meteo data are summarised to days. ",
                 "The hours per day at or above this humidity are the wet period ",
                 "that infection needs. Hourly downloads are larger, so a run ",
                 "takes a little longer. (A CSV carries its own rh_wet_hours.)"),
        tags$details(
          tags$summary("NASA POWER options"),
          selectInput("nasa_community", "Community", c("AG", "RE", "SB"),
                      selected = cfg_get(c("weather", "nasa_community"), "AG")),
          selectInput("nasa_time_standard", "Time standard", c("LST", "UTC"),
                      selected = cfg_get(c("weather", "nasa_time_standard"), "LST"))
        )
      ),
      section("Simulation",
        numericInput("lead_days", "Pre-sowing lead days",
                     cfg_get(c("simulation", "spore_dynamics_lead_days"), 60),
                     min = 0, max = 365),
        numericInput("residue", "Initial residue inoculum",
                     cfg_get(c("simulation", "blast_initial_residue_inoculum"), 100),
                     min = 0),
        checkboxInput("stop_at_maturity", "Stop at physiological maturity",
                      cfg_flag(c("simulation", "stop_at_maturity"), TRUE)),
        numericInput("time_step", "Time step (days)",
                     cfg_get(c("simulation", "time_step"), 0.1),
                     min = 0.02, max = 1, step = 0.02)
      ),
      section("Alternative host",
        checkboxInput("alt_enabled", "Include alternative host",
                      cfg_flag(c("alternative_host", "enabled"), TRUE)),
        numericInput("alt_lai", "Leaf area index",
                     cfg_get(c("alternative_host", "lai"), 10), min = 0),
        numericInput("alt_area", "Initial infectious area",
                     cfg_get(c("alternative_host", "initial_infectious_area"), 0.1),
                     min = 0, step = 0.01),
        numericInput("alt_mult", "Spore production multiplier",
                     cfg_get(c("alternative_host", "spore_production_multiplier"), 2),
                     min = 0, step = 0.1)
      ),
      section("Infection conditions", open = TRUE,
        numericInput("min_wet_hours", "Minimum wet period (hours/day with high humidity)",
                     cfg_get(c("pathogen", "infection_min_wet_hours"),
                             model_params$infection_min_wet_hours),
                     min = 0, max = 24, step = 0.5),
        helpText("Infection (leaf, alternative host and spike) needs a favorable ",
                 "temperature AND at least this many hours per day with relative ",
                 "humidity at or above the wet threshold (Weather section)."),
        numericInput("inf_tmin", "Infection temperature, minimum (\u00b0C)",
                     cfg_get(c("pathogen", "infection_Tmin_C"), model_params$leaf_Tmin),
                     step = 0.5),
        numericInput("inf_topt", "Infection temperature, optimum (\u00b0C)",
                     cfg_get(c("pathogen", "infection_Topt_C"), model_params$leaf_Topt),
                     step = 0.5),
        numericInput("inf_tmax", "Infection temperature, maximum (\u00b0C)",
                     cfg_get(c("pathogen", "infection_Tmax_C"), model_params$leaf_Tmax),
                     step = 0.5),
        helpText("The temperature response rises from 0 at the minimum to 1 at the ",
                 "optimum and falls to 0 at the maximum, applied to the daily mean ",
                 "temperature.")
      ),
      section("Pathogen (advanced)",
        numericInput("spike_start", "Heading window start (°C d)",
                     pathogen_default("spike_heading_start_TT", "blast_spike_heading_start_TT")),
        numericInput("spike_peak", "Heading window peak (°C d)",
                     pathogen_default("spike_heading_peak_TT", "blast_spike_heading_peak_TT")),
        numericInput("spike_end", "Heading window end (°C d)",
                     pathogen_default("spike_heading_end_TT", "blast_spike_heading_end_TT")),
        numericInput("kernel_start", "Kernel formation (°C d)",
                     pathogen_default("kernel_start_TT", "blast_kernel_start_TT")),
        numericInput("hard_dough", "Hard dough (°C d)",
                     pathogen_default("hard_dough_TT", "blast_hard_dough_TT")),
        numericInput("leaf_max_frac", "Maximum diseased leaf fraction",
                     pathogen_default("leaf_max_diseased_fraction", "leaf_max_diseased_fraction"),
                     min = 0, max = 1, step = 0.01),
        numericInput("leaf_lesion", "Maximum lesion size (LAI units)",
                     pathogen_default("leaf_max_lesion_size", "leaf_max_lesion_size"),
                     step = 0.0005),
        numericInput("leaf_crowding", "Crowding exponent",
                     pathogen_default("leaf_infection_crowding_exponent",
                                      "leaf_infection_crowding_exponent"),
                     min = 1, step = 0.5)
      ),
      section("Soil layers",
        textInput("soil_FC", "Field capacity (4 layers)",
                  cfg_vector_text(c("soil", "FC"), soil_layers_df$FC)),
        textInput("soil_WP", "Wilting point",
                  cfg_vector_text(c("soil", "WP"), soil_layers_df$WP)),
        textInput("soil_SAT", "Saturation",
                  cfg_vector_text(c("soil", "SAT"), soil_layers_df$SAT)),
        helpText("Layer thicknesses are fixed at 0.1, 0.1, 0.2 and 0.4 m.")
      ),
      section("Report",
        selectInput("report_lang", "Language", c("English" = "en", "Portugu\u00eas" = "pt"),
                    selected = cfg_get(c("report", "language"), "en")),
        textInput("report_author", "Author", cfg_get(c("report", "author"), "")),
        textInput("report_recipients", "Recipients (separated by semicolons)",
                  paste(unlist(cfg_get(c("report", "recipients"), list())), collapse = "; ")),
        textInput("risk_limits", "Risk class limits (kernel infection, 5 values)",
                  cfg_vector_text(c("report", "risk_thresholds"), c(0.05, 0.1, 0.2, 0.3, 0.4))),
        numericInput("report_block_days", "Forecast table: days per period",
                     value = cfg_get(c("report", "forecast_block_days"), 21), min = 1, max = 60, step = 1),
        helpText("Classes: none, low, moderate, high, very high, extreme. ",
                 "Run the simulation, then use 'Download report' below."),
        downloadButton("dl_report", "Download report (HTML)")
      ),
      section("Parameter overrides",
        textAreaInput("overrides", NULL, rows = 3, placeholder = "leaf_infection_rate: 0.3"),
        helpText("One 'name: value' per line. Names are model parameters; ",
                 "unknown names are reported in the run log.")
      ),
      br(),
      actionButton("run", "Run simulation", class = "btn-primary btn-lg w-100"),
      br(), br(),
      downloadButton("dl_settings", "Download settings (YAML)")
    ),
    mainPanel(
      width = 8,
      conditionalPanel(
        "output.has_run",
        div(style = "margin-bottom:10px;",
            downloadButton("dl_report_main", "Download simulation report (HTML)", class = "btn-primary"),
            span(class = "note", style = "margin-left:10px;",
                 "Language, author and class limits: 'Report' section on the left."))
      ),
      tabsetPanel(
        id = "tabs",
        tabPanel("Overview", br(), uiOutput("overview")),
        tabPanel("Forecast table", br(), uiOutput("forecast_ui")),
        tabPanel("Disease", br(),
          conditionalPanel("!output.has_run", p(class = "note", "Run the simulation first.")),
          conditionalPanel("output.has_run", plotOutput("disease_plot", height = "980px"))),
        tabPanel("Crop and soil", br(),
          conditionalPanel("!output.has_run", p(class = "note", "Run the simulation first.")),
          conditionalPanel("output.has_run", plotOutput("crop_plot", height = "860px"))),
        tabPanel("Weather", br(),
          conditionalPanel("!output.has_run", p(class = "note", "Run the simulation first.")),
          conditionalPanel("output.has_run", plotOutput("weather_plot", height = "760px"))),
        tabPanel("Run details", br(), uiOutput("details_ui"))
      )
    )
  )
)

# --- Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  state <- reactiveVal(NULL)

  collect_inputs <- function() {
    mode <- input$weather_mode
    list(
      location_name = input$location_name, latitude = input$latitude,
      longitude = input$longitude, timezone = input$timezone,
      sowing_date = input$sowing_date, cultivar = input$cultivar,
      susceptibility = input$susceptibility,
      TT_FlagLeaf = input$TT_FlagLeaf, TT_Anthesis = input$TT_Anthesis,
      TT_GrainFill = input$TT_GrainFill, TTM = input$TTM,
      weather_mode = mode,
      hindcast = isTRUE(input$hindcast) && !identical(mode, "blended"),
      hindcast_date = input$hindcast_date,
      forecast_days = input$forecast_days,
      nasa_community = input$nasa_community,
      nasa_time_standard = input$nasa_time_standard,
      csv_path = if (!is.null(input$csv_file)) input$csv_file$datapath else NULL,
      lead_days = input$lead_days, residue = input$residue,
      stop_at_maturity = isTRUE(input$stop_at_maturity), time_step = input$time_step,
      alt_enabled = isTRUE(input$alt_enabled), alt_lai = input$alt_lai,
      alt_area = input$alt_area, alt_mult = input$alt_mult,
      rh_threshold = input$rh_threshold, min_wet_hours = input$min_wet_hours,
      inf_tmin = input$inf_tmin, inf_topt = input$inf_topt, inf_tmax = input$inf_tmax,
      spike_start = input$spike_start, spike_peak = input$spike_peak,
      spike_end = input$spike_end,
      kernel_start = input$kernel_start, hard_dough = input$hard_dough,
      leaf_max_frac = input$leaf_max_frac, leaf_lesion = input$leaf_lesion,
      leaf_crowding = input$leaf_crowding,
      soil_FC = input$soil_FC, soil_WP = input$soil_WP, soil_SAT = input$soil_SAT,
      overrides = input$overrides
    )
  }

  # --- Saved configurations ------------------------------------------------------
  fill_form <- function(merged) {
    get <- function(...) {
      v <- merged
      for (k in c(...)) { v <- if (is.list(v)) v[[k]] else NULL; if (is.null(v)) return(NULL) }
      v
    }
    txt <- function(id, ...) { v <- get(...); if (!is.null(v)) updateTextInput(session, id, value = as.character(v)) }
    num <- function(id, ...) { v <- get(...); if (!is.null(v)) updateNumericInput(session, id, value = as.numeric(v)) }
    vec <- function(id, ...) { v <- get(...); if (!is.null(v)) updateTextInput(session, id, value = paste(unlist(v), collapse = ", ")) }
    txt("location_name", "location", "name"); txt("timezone", "location", "timezone")
    num("latitude", "location", "latitude"); num("longitude", "location", "longitude")
    if (!is.null(get("crop", "sowing_date")))
      updateDateInput(session, "sowing_date", value = as.Date(as.character(get("crop", "sowing_date"))))
    txt("cultivar", "crop", "cultivar")
    if (!is.null(get("crop", "cultivar_susceptibility")))
      updateSliderInput(session, "susceptibility", value = as.numeric(get("crop", "cultivar_susceptibility")))
    for (id in c("TT_FlagLeaf", "TT_Anthesis", "TT_GrainFill", "TTM")) num(id, "crop", id)
    vec("soil_FC", "soil", "FC"); vec("soil_WP", "soil", "WP"); vec("soil_SAT", "soil", "SAT")
    num("spike_start", "pathogen", "spike_heading_start_TT")
    num("spike_peak", "pathogen", "spike_heading_peak_TT")
    num("spike_end", "pathogen", "spike_heading_end_TT")
    num("kernel_start", "pathogen", "kernel_start_TT")
    num("hard_dough", "pathogen", "hard_dough_TT")
    num("min_wet_hours", "pathogen", "infection_min_wet_hours")
    num("inf_tmin", "pathogen", "infection_Tmin_C"); num("inf_topt", "pathogen", "infection_Topt_C")
    num("inf_tmax", "pathogen", "infection_Tmax_C")
    invisible(NULL)
  }

  observeEvent(input$load_config, {
    req(input$saved_config)
    merged <- tryCatch(
      read_site_config(file.path(configs_dir, paste0(input$saved_config, ".yaml")),
                       yaml_path, locations_path, cultivars_path)$cfg,
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 12); NULL
      }
    )
    if (is.null(merged)) return()
    fill_form(merged)
    showNotification(paste("Loaded", input$saved_config), type = "message")
  })

  observeEvent(input$cultivar_pick, {
    req(nzchar(input$cultivar_pick))
    entry <- read_cultivar_entry(cultivars_path, input$cultivar_pick)
    if (is.null(entry)) {
      showNotification("That cultivar set was not found in cultivars.yaml.", type = "error")
      return()
    }
    fill_form(entry)
    showNotification(paste("Cultivar set applied:", entry$crop$cultivar), type = "message")
  })

  pending_save <- reactiveVal(NULL)
  write_site_file <- function(path, settings) {
    keep <- prune_null(settings$yaml[c("location", "crop", "soil")])
    keep$pathogen <- prune_null(settings$yaml$pathogen[c(
      "spike_heading_start_TT", "spike_heading_peak_TT", "spike_heading_end_TT",
      "kernel_start_TT", "hard_dough_TT")])
    keep$crop$sowing_date <- NULL     # the date is carried by the file name
    writeLines(c(
      "# Saved from the dashboard. Location and sowing date come from the file name;",
      "# other settings come from wheat_blast_model_config.yaml.",
      strsplit(yaml::as.yaml(keep), "\n")[[1]]
    ), path)
    updateSelectInput(session, "saved_config", choices = config_choices(),
                      selected = sub("\\.yaml$", "", basename(path)))
    showNotification(paste("Saved", basename(path)), type = "message")
  }
  observeEvent(input$save_config, {
    settings <- tryCatch(build_settings(collect_inputs()), error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 12); NULL
    })
    if (is.null(settings)) return()
    slug <- slugify_location(input$location_name)
    if (!nzchar(slug)) {
      showNotification("Give the location a name first.", type = "error"); return()
    }
    dir.create(configs_dir, showWarnings = FALSE)
    path <- file.path(configs_dir, sprintf("%s_%s.yaml", slug, format(as.Date(input$sowing_date))))
    if (file.exists(path)) {
      pending_save(list(path = path, settings = settings))
      showModal(modalDialog(
        title = "Replace this configuration?",
        paste(basename(path), "already exists."),
        footer = tagList(modalButton("Cancel"),
                         actionButton("confirm_save", "Replace", class = "btn-primary"))
      ))
    } else write_site_file(path, settings)
  })
  observeEvent(input$confirm_save, {
    p <- pending_save(); removeModal()
    if (!is.null(p)) write_site_file(p$path, p$settings)
    pending_save(NULL)
  })

  observeEvent(input$run, {
    settings <- tryCatch(
      build_settings(collect_inputs()),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 12)
        NULL
      }
    )
    if (is.null(settings)) return()
    run <- NULL
    withProgress(
      message = "Running the model",
      detail = "Weather download and simulation can take a minute.",
      value = 0.3,
      { run <- run_model(settings) }
    )
    state(c(run, list(settings = settings, finished = Sys.time())))
    if (!is.null(run$error)) {
      showNotification(paste("The run failed:", run$error), type = "error",
                       duration = NULL)
    } else {
      showNotification("Simulation finished.", type = "message", duration = 4)
    }
  })

  res <- reactive({
    s <- state()
    if (is.null(s)) NULL else s$result
  })
  fc <- reactive({
    r <- res()
    if (is.null(r)) NULL else r$forecast_summary
  })

  output$has_run <- reactive(!is.null(res()))
  outputOptions(output, "has_run", suspendWhenHidden = FALSE)

  # Overview ----------------------------------------------------------------------
  output$overview <- renderUI({
    s <- state()
    if (is.null(s)) {
      return(div(class = "empty",
                 h4("Ready"),
                 p("Set the options on the left and press Run simulation. ",
                   "Defaults come from wheat_blast_model_config.yaml.")))
    }
    if (!is.null(s$error)) {
      return(div(class = "alert alert-danger",
                 strong("The run failed. "), s$error,
                 " The Run details tab has the log."))
    }
    f <- s$result$forecast_summary
    tagList(
      headline_ui(s$result, f),
      plotOutput("headline_plot", height = if (is.null(f)) "320px" else "760px"),
      p(class = "note",
        "Kernel infection is in relative model units, not a probability. ",
        "Forecast rain and wetness are usually less reliable beyond about a week, ",
        "so read the later days as scenarios. The shaded band marks forecast days."),
      if (length(s$settings$notes)) p(class = "note", paste(s$settings$notes, collapse = " "))
    )
  })

  output$headline_plot <- renderPlot({
    r <- res()
    req(r)
    f <- r$forecast_summary
    if (!is.null(f)) plot_outlook(f, r$params) else plot_kernel_progress(r)
  }, res = 96)

  # Forecast table ----------------------------------------------------------------
  output$forecast_ui <- renderUI({
    if (is.null(state())) return(p(class = "note", "Run the simulation first."))
    if (is.null(fc())) {
      return(p(class = "note",
               "This run has no Open-Meteo forecast days (NASA-only, CSV, hindcast, ",
               "or maturity reached before the forecast)."))
    }
    tagList(
      h5("Summary"), tableOutput("forecast_summary_table"),
      h5("Daily values for the forecast days"), tableOutput("forecast_daily_table"),
      p(class = "note", "State columns are values at the start of each day; the ",
        "change columns are next-day start minus this-day start."),
      downloadButton("dl_forecast", "Download daily table (CSV)")
    )
  })

  output$forecast_summary_table <- renderTable({
    req(fc())
    metric_table(fc()$summary)
  }, striped = TRUE, spacing = "s")

  output$forecast_daily_table <- renderTable({
    req(fc())
    w <- fc()$window
    data.frame(
      Date = format(w$Date), Rain_mm = round(w$Rain_mm, 1),
      Tmin = round(w$Tmin, 1), Tmean = round(w$Tmean, 1), Tmax = round(w$Tmax, 1),
      RH_wet_hours = round(w$RH_Wet_Hours, 1),
      Wet_period_met = ifelse(w$Wet_Period_Met, "yes", "no"),
      Temperature_response = round(w$Infection_Temp_Factor, 2),
      Heading_susceptibility = round(w$Heading_Susceptibility, 2),
      Conditions_met = ifelse(w$Infection_Conditions_Met, "yes", "no"),
      Kernel_infection = signif(w$Kernel_Infection, 4),
      Kernel_infection_change = signif(w$Kernel_Infection_Change, 4)
    )
  }, striped = TRUE, spacing = "s", na = "–")

  # Season plots ------------------------------------------------------------------
  output$disease_plot <- renderPlot({ req(res()); plot_disease(res()) }, res = 96)
  output$crop_plot <- renderPlot({ req(res()); plot_crop(res()) }, res = 96)
  output$weather_plot <- renderPlot({ req(res()); plot_weather(res()) }, res = 96)

  # Run details -------------------------------------------------------------------
  output$details_ui <- renderUI({
    s <- state()
    if (is.null(s)) return(p(class = "note", "Run the simulation first."))
    tagList(
      p(class = "note", "Finished ", format(s$finished, "%Y-%m-%d %H:%M:%S")),
      h5("Run log (messages, warnings, printed output)"),
      verbatimTextOutput("run_log"),
      if (!is.null(s$result)) tagList(
        h5("Summary"), tableOutput("run_summary"),
        h5("Phenology"), tableOutput("phenology_table"),
        downloadButton("dl_output", "Model output (CSV)"),
        downloadButton("dl_weather", "Weather used (CSV)")
      ),
      h5("Settings used (YAML)"),
      verbatimTextOutput("yaml_used")
    )
  })
  output$run_log <- renderText({
    s <- state()
    req(s)
    if (length(s$log)) paste(s$log, collapse = "\n") else "(nothing was logged)"
  })
  output$run_summary <- renderTable({
    req(res())
    metric_table(res()$wheat_blast_summary)
  }, striped = TRUE, spacing = "s")
  output$phenology_table <- renderTable({
    req(res())
    p <- res()$phenology
    data.frame(Stage = p$Stage, Thermal_time = p$TT_Threshold,
               Date = format(as.Date(p$Date)))
  }, striped = TRUE, spacing = "s")
  output$yaml_used <- renderText({
    s <- state()
    req(s)
    settings_to_yaml(s$settings)
  })

  # Downloads ---------------------------------------------------------------------
  output$dl_settings <- downloadHandler(
    filename = function() "wheat_blast_settings.yaml",
    content = function(file) {
      text <- tryCatch(
        settings_to_yaml(build_settings(collect_inputs())),
        error = function(e) paste0("# The current inputs are invalid: ",
                                   conditionMessage(e), "\n")
      )
      writeLines(text, file)
    }
  )
  output$dl_forecast <- downloadHandler(
    filename = function() "forecast_daily.csv",
    content = function(file) {
      req(fc())
      utils::write.csv(fc()$daily, file, row.names = FALSE)
    }
  )
  report_filename <- function() {
      paste0("wheat_blast_report_", slugify_location(input$location_name), "_",
             format(as.Date(input$sowing_date)), ".html")
    }
  report_content <- function(file) {
      fail <- function(message_text) {
        showNotification(message_text, type = "error", duration = NULL)
        writeLines(paste0("<p>Report not created: ", htmltools::htmlEscape(message_text), "</p>"), file)
      }
      s <- state()
      if (is.null(s) || is.null(s$result)) return(fail("Run the simulation first."))
      tryCatch({
        limits <- parse_numeric_vector(input$risk_limits, 5, "Risk class limits")
        validate_risk_thresholds(limits)
        block_days <- suppressWarnings(as.integer(input$report_block_days))
        if (length(block_days) != 1L || is.na(block_days) || block_days < 1L || block_days > 60L) {
          stop("The forecast period length must be a whole number between 1 and 60 days.")
        }
        soil_used <- soil_layers_df
        soil_used$FC <- as.numeric(unlist(s$settings$yaml$soil$FC))
        soil_used$WP <- as.numeric(unlist(s$settings$yaml$soil$WP))
        recipients <- trimws(unlist(strsplit(input$report_recipients %||% "", ";")))
        withProgress(message = "Building the report", value = 0.5, {
          render_simulation_report(
            s$result, file, language = input$report_lang,
            author = input$report_author, recipients = recipients[nzchar(recipients)],
            cultivar = s$settings$yaml$crop$cultivar,
            susceptibility = s$settings$yaml$crop$cultivar_susceptibility,
            soil_layers = soil_used, thresholds = limits,
            block_days = block_days
          )
        })
      }, error = function(e) fail(paste("The report failed:", conditionMessage(e))))
    }
  output$dl_report <- downloadHandler(filename = report_filename, content = report_content)
  output$dl_report_main <- downloadHandler(filename = report_filename, content = report_content)
  output$dl_output <- downloadHandler(
    filename = function() "simulation_results.csv",
    content = function(file) {
      req(res())
      utils::write.csv(res()$output, file, row.names = FALSE)
    }
  )
  output$dl_weather <- downloadHandler(
    filename = function() "weather_used.csv",
    content = function(file) {
      req(res())
      utils::write.csv(res()$weather, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
