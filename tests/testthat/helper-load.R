# Sources the light modules (base R only) so the core tests need only testthat.
# The smoke test sources the heavier modules itself and skips if deSolve is missing.
project_root <- normalizePath(testthat::test_path("..", ".."))

source_module <- function(file) {
  source(file.path(project_root, "R", file))
}

for (module in c("01_utils.R", "02_params.R", "03_response_functions.R",
                 "04_weather.R", "09_forecast.R", "11_site_configs.R", "12_report.R")) {
  source_module(module)
}
