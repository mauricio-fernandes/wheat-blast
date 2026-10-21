# Start the dashboard:  Rscript run_app.R   (or source it from RStudio)
# Needs the shiny package; bslib is optional (nicer theme).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
project_root <- if (length(file_arg)) dirname(normalizePath(file_arg[1])) else getwd()
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Install shiny first: install.packages('shiny')", call. = FALSE)
}
shiny::runApp(file.path(project_root, "app"), launch.browser = TRUE)
