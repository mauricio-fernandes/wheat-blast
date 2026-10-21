# ============================================================================
# 00_packages.R
# Package attachments and the required-package check.
# ============================================================================

library(deSolve)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(reshape2)
library(dplyr)

# Called by run_forecast.R after the modules are loaded (the original script
# stopped at this point when a required package was missing).
check_required_packages <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for Open-Meteo JSON. Install it with install.packages('jsonlite').", call. = FALSE)
  }
  if (!requireNamespace("nasapower", quietly = TRUE)) {
    stop("Package 'nasapower' is required for NASA POWER downloads. Install it with install.packages('nasapower').", call. = FALSE)
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to read the forecast configuration. Install it with install.packages('yaml').", call. = FALSE)
  }
}
