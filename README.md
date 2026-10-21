# Wheat blast risk model

A simulation model of wheat blast (*Pyricularia oryzae*, Triticum pathotype) written in R. It combines a daily soil water balance, wheat development from thermal time, and a disease model for leaves, an alternative host and the wheat spike and kernels. Weather is hourly: NASA POWER up to its last complete day, then Open-Meteo for the most recent days and a 16-day forecast. The main output is **kernel infection in relative units**, grouped into risk classes from None to Extreme (severe). The model comes with a Shiny dashboard and an HTML simulation report in English or Portuguese.

Developed as part of an international research project.

## Status

This is a research prototype.

- The parameter values (the temperature range for infection, the minimum of 8 wet hours per day, the cultivar parameter sets and the risk class limits) are **preliminary and have not been calibrated or validated against field data**.
- Risk classes are relative to this model. They are not a predicted incidence and should not be the only basis for a management decision.
- Live runs depend on the availability of the NASA POWER and Open-Meteo services.

## Quick start

Install R, then the packages:

    install.packages(c("deSolve", "ggplot2", "gridExtra", "lubridate", "reshape2", "dplyr",
                       "jsonlite", "nasapower", "yaml", "shiny", "testthat",
                       "rmarkdown", "knitr"))

(`rmarkdown` and `knitr` also need [pandoc](https://pandoc.org/installing.html) to build the report.)

    Rscript run_forecast.R passo-fundo --report    # one location, with an HTML report
    Rscript run_app.R                              # the dashboard
    Rscript tests/run_tests.R                      # the tests

`wheat_blast_model_config.yaml` must stay next to `run_forecast.R`. Locations, cultivars and sowing dates are described in "Configurations by location and sowing date" below.

## Acknowledgment

This model was adapted from the original FHB and generic foliar-disease teaching model developed by J. M. Fernandes for participants in the international training course "Wheat Diseases and Resistance Breeding", CIMMYT, Mexico, August 3-22, 2025.

## Layout

| File | Contents |
|---|---|
| `run_forecast.R` | Entry point: finds its folder, sources `R/`, checks packages, loads the YAML, runs the simulation |
| `R/00_packages.R` | `library()` calls and `check_required_packages()` |
| `R/01_utils.R` | Unit conversions, `clamp01`, `%\|\|%` |
| `R/12_report.R` | Kernel-infection risk classes and the HTML report renderer (template in `report/`) |
| `R/11_site_configs.R` | Per-location, per-sowing-date config files layered on the base config |
| `R/02_params.R` | Default config, soil layers, `model_params`, `load_yaml_configuration()` |
| `R/03_response_functions.R` | Wet-period gate, root depth, water stress, cardinal temperature, heading susceptibility, AUDPC |
| `R/04_weather.R` | NASA POWER, Open-Meteo, blending, gap filling, preprocessing |
| `R/05_soil_water.R` | Daily soil water balance |
| `R/06_disease_ode.R` | Crop/disease ODE, initial state, solver wrapper |
| `R/07_phenology.R` | Stage dates from thermal time |
| `R/08_plots.R` | Plots |
| `R/09_forecast.R` | Forecast-window summary and printing |
| `R/10_simulation.R` | `run_simulation()` |
| `run_app.R` | Starts the Shiny dashboard |
| `app/app.R`, `app/helpers.R`, `app/plots_app.R` | Dashboard UI/server, input validation and YAML export, single-axis charts |

Modules are sourced in numeric order into the global environment, so function
defaults such as `run_simulation(config = simulation_config)` behave exactly as
in the single file. To add a module, give it the next two-digit prefix.

## Tests

    Rscript tests/run_tests.R     # needs testthat

- `test-wet-period.R`: wet-period gate, temperature response, both-conditions rule, obsolete-setting warning
- `test-gap-filling.R`: gap filling and weather preprocessing (`rh_wet_hours` required)
- `test-forecast-summary.R`: forecast-window summary on a synthetic 10-day record
- `test-yaml-config.R`: thermal time under `crop:`, infection settings and humidity threshold (needs `yaml`)
- `test-report.R`: risk classes and limits, report text in both languages, report settings, report rendering (needs pandoc)
- `test-site-configs.R`: file-name parsing, selection, layering and consistency checks (needs `yaml`)
- `test-hourly-weather.R`: hourly-to-daily aggregation, humid-hours variable, Open-Meteo parsing, archive bridge (no network)
- `test-app-helpers.R`: dashboard input parsing, validation, YAML round trip, offline `run_model()` (needs `yaml`, and `deSolve` for the run)
- `test-smoke-simulation.R`: offline end-to-end run on synthetic CSV weather (needs `deSolve`)

The first three need only base R and testthat. No test touches the network.

## Hourly weather and the infection rule

The model downloads **hourly** NASA POWER (`T2M, RH2M, T2MDEW, PRECTOTCORR,
ALLSKY_SFC_SW_DWN`) and Open-Meteo (`temperature_2m, relative_humidity_2m,
dew_point_2m, precipitation, shortwave_radiation`) data and summarises them to daily
values in `aggregate_hourly_weather()`:

| Daily variable | From the hourly data |
|---|---|
| `tmax`, `tmin` | maximum and minimum temperature |
| `dpt` | mean dew point (informational only) |
| `rain` | sum of hourly rain (mm) |
| `srad` | sum of hourly W m-2 x 3600 s (MJ m-2 d-1) |
| `rh_wet_hours` | hours with relative humidity **at or above** `weather.rh_wet_threshold_pct` (default 90); scaled to 24 h if a few hours are missing |

A day with fewer than 20 valid hours of a variable is NA for that variable, so
today's partial NASA day is not treated as complete (the blend then switches to
Open-Meteo). If Open-Meteo's recent endpoint returns null rows for old days, the
Open-Meteo archive bridges the days between NASA's last complete day and the first
complete Open-Meteo day. Short gaps (up to 3 days) are interpolated and flagged.

**Infection rule (replaces the canopy-moisture index and the dew-point response).**
Leaf, alternative-host and spike infection, and secondary sporulation on lesions,
need **both**

1. a favorable temperature: the cardinal response `infection_Tmin_C` /
   `infection_Topt_C` / `infection_Tmax_C` (defaults 15 / 28 / 30 °C) applied to the
   daily mean temperature, and
2. a wet period: `rh_wet_hours >= pathogen.infection_min_wet_hours` (default 8 h).

The wet-period gate is 0 or 1 for the whole day. The temperature thresholds and
the 8 h minimum are placeholders to calibrate. There is no wetness state variable
any more. The run summary reports `Mean_RH_Wet_Hours` and `Days_Wet_Period_Met`; the
output has `RH_Wet_Hours`, `Wet_Period_Met`, `Infection_Temp_Factor` and
`Infection_Favorability`; the forecast table adds the same fields per day.

A **CSV** weather file needs an `rh_wet_hours` column (hours per day with RH at or
above the threshold) in addition to `date, rain, tmax, tmin, dpt, srad`.
Old YAML files that still contain `dew_*`, `leaf_wetness_method`,
`wetness_hours_*`, `spike_moisture_threshold` or `weather.hourly` keys load with a
warning that those keys are ignored.

Notes: NASA POWER "LST" days are local *solar* days, a few tens of minutes off
the civil-time days Open-Meteo returns. Hourly downloads are larger, so a run takes
longer. The temperature response uses the daily mean, which is warmer than the
temperature during the night-time wet hours.

## Configurations by location and sowing date

```
wheat_blast_model_config.yaml   shared defaults (pathogen, infection rule, crop TT, ...)
locations.yaml                  name, coordinates, time zone and optional soil per location slug
configs/passo-fundo_2026-06-20.yaml    <location-slug>_<YYYY-MM-DD>.yaml
```

A site file only holds what differs from the base (often just the cultivar). Location
and sowing date come from the file name: the slug is looked up in `locations.yaml`
and the date becomes `crop.sowing_date`. If the file also states a sowing date or a
`location:` slug that disagrees with its name, the run stops. A site file may carry an
inline `location:` block, a `soil:` block or any other section; these override the base.
Overriding a key with `null` does not work (it keeps the base value); give a value.

```
Rscript run_forecast.R --list                   # show the available configs
Rscript run_forecast.R passo-fundo_2026-06-20   # one exact config
Rscript run_forecast.R passo-fundo              # latest sowing date on or before today
Rscript run_forecast.R --all                    # every config, no plots, combined summary
Rscript run_forecast.R                          # the base config alone, as before
```

Results go to `outputs/<config name>/` (`forecast_summary.csv`, `forecast_daily.csv`,
`simulation_results.csv`, `phenology_results.csv`). `--all` also writes
`outputs/all_forecast_summaries_<date>.csv`, one row per config; a failing config is
reported and the others still run.

### Cultivar parameter sets (`cultivars.yaml`)

Define a cultivar's thermal-time, susceptibility and heading/kernel timings once and
reuse them by id. Each entry holds a `crop:` and/or `pathogen:` block with the same keys
as the base config; `crop.cultivar` inside the entry is the display name.

```yaml
# cultivars.yaml
early-example:
  crop: { cultivar: Early example, cultivar_susceptibility: 0.5, TT_Anthesis: 1200 }
  pathogen: { spike_heading_peak_TT: 1200 }

# configs/artigas_2026-06-26.yaml
crop:
  cultivar: early-example      # an id from cultivars.yaml
  TT_Anthesis: 1250            # optional: override one value for this site
```

Layering is base, then the cultivar entry, then the site file. A `crop.cultivar` that is
not an id is a plain label and changes nothing. If it looks like an id (lowercase,
hyphens) but is missing from `cultivars.yaml`, the run warns, which catches typos.
The values shipped in `cultivars.yaml` are placeholders.
In the dashboard, the Crop section has a "Cultivar set" dropdown listing these ids; choosing one fills the
cultivar name, susceptibility, thermal-time and heading/kernel fields (the sowing date and location stay).

In the dashboard, the first section lists `configs/`: **Load into the form** fills the
location, crop, soil and infection fields; **Save current form** writes the location,
crop and soil sections and the heading/kernel timings to `configs/<location>_<sowing date>.yaml`, named from the text
before the first comma of the location name, and asks before replacing a file.

## Simulation report with kernel-infection risk classes

An HTML report summarises a run: location and weather, soil, crop development, spore
dynamics, the infection conditions (wet hours and temperature), kernel infection with
the risk classes, a summary of the forecast window (periods of up to `report: forecast_block_days` days, default 21, and the days when all infection conditions were met), recommendations, limitations and a
conclusion. It comes in English or Portuguese (`report/report_text.yaml` holds the
text, `report/simulation_report.Rmd` the layout).

Risk class from **kernel infection (relative units)**; a value exactly at a limit
belongs to the higher class. The limits are preliminary and not calibrated:

| Class | Kernel infection |
|---|---|
| None | below 0.05 |
| Low | 0.05 to below 0.1 |
| Moderate | 0.1 to below 0.2 |
| High | 0.2 to below 0.3 |
| Very high | 0.3 to below 0.4 |
| Extreme (severe) | 0.4 or more |

The headline class uses the **peak** kernel infection of the whole period, because the
value can decline later (the model has a recovery term). The report also gives the
latest value, the highest value in the forecast window and the first day each class was
reached. Change the limits in the YAML (`report: risk_thresholds`) or in the dashboard.

```
Rscript run_forecast.R passo-fundo --report            # writes outputs/<name>/report_<name>.html
Rscript run_forecast.R passo-fundo --report --lang=pt  # Portuguese
Rscript run_forecast.R --all --report                  # one report per config
```

In the dashboard, the **Report** section has language, author, recipients, the class
limits and the length of the forecast periods. After a run, the button "Download simulation
report (HTML)" appears above the result tabs (the same button is also in the Report section). The `report:` block of
the YAML sets the defaults (language, author, recipients, limits).

The report needs the `rmarkdown`, `knitr` and `ggplot2` packages and **pandoc**, which is
included with RStudio. Without RStudio, install pandoc from https://pandoc.org/installing.html
(on a Mac with Homebrew: `brew install pandoc`).

## Dashboard

    Rscript run_app.R          # or: shiny::runApp("app")

Needs `shiny`, plus the model packages (`ggplot2`, `gridExtra`, `yaml`, `deSolve`, ...).
`bslib` is optional and only changes the theme. Defaults are read from
`wheat_blast_model_config.yaml`; the inputs are written to a temporary YAML and loaded
by the same `load_yaml_configuration()`, so the dashboard and the script share one code path.

- Weather: NASA POWER + Open-Meteo forecast, NASA POWER only, or an uploaded CSV
  (columns `date, rain, tmax, tmin, dpt, srad`). A hindcast end date is available for
  the last two.
- Tabs: Overview (forecast-window tiles and outlook chart), Forecast table, Disease,
  Crop and soil, Weather, Run details (log, settings used, CSV downloads).
- "Download settings (YAML)" exports the current inputs as a config file for
  `run_forecast.R`.
- Charts are static images (no hover); the table view and CSV downloads carry the numbers.
  Light theme only.
