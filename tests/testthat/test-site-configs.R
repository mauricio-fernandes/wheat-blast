# Site configs: <location>_<YYYY-MM-DD>.yaml layered on the base config. Needs yaml.

make_project <- function() {
  skip_if_not_installed("yaml")
  root <- tempfile("proj"); dir.create(file.path(root, "configs"), recursive = TRUE)
  writeLines(c("location:", "  name: Base place", "  latitude: -10", "  longitude: -50",
               "crop:", "  sowing_date: 2026-01-01", "  cultivar: Base cultivar",
               "  cultivar_susceptibility: 0.7",
               "weather:", "  nasa_power_only: false", "  nasa_power_open_meteo: true",
               "pathogen:", "  infection_min_wet_hours: 8"),
             file.path(root, "base.yaml"))
  writeLines(c("passo-fundo:", "  name: Passo Fundo", "  latitude: -28.262",
               "  longitude: -52.406", "  timezone: America/Sao_Paulo",
               "  soil:", "    FC: [0.30, 0.30, 0.30, 0.30]"),
             file.path(root, "locations.yaml"))
  root
}
put <- function(root, name, lines) writeLines(lines, file.path(root, "configs", name))

test_that("names parse and invalid names are rejected", {
  p <- parse_site_config_name("configs/passo-fundo_2026-06-20.yaml")
  expect_equal(p$location, "passo-fundo")
  expect_equal(p$sowing_date, as.Date("2026-06-20"))
  expect_null(parse_site_config_name("passo-fundo.yaml"))
  expect_null(parse_site_config_name("Passo_2026-06-20.yaml"))
  expect_null(parse_site_config_name("passo-fundo_2026-02-31.yaml"))
})

test_that("slugify_location keeps the first part, ASCII and lowercase", {
  expect_equal(slugify_location("Passo Fundo, Rio Grande do Sul, Brazil"), "passo-fundo")
  expect_equal(slugify_location("São Paulo"), "sao-paulo")
})

test_that("list and resolve pick the right file", {
  root <- make_project(); d <- file.path(root, "configs")
  for (n in c("passo-fundo_2026-06-20.yaml", "passo-fundo_2026-07-10.yaml",
              "passo-fundo_2026-11-01.yaml", "londrina_2026-05-15.yaml")) put(root, n, "crop: {}")
  put(root, "notes.yaml", "x: 1")   # ignored: not a valid name
  tab <- list_site_configs(d)
  expect_equal(nrow(tab), 4)
  expect_equal(tab$name[1], "londrina_2026-05-15")
  expect_equal(resolve_site_config("passo-fundo_2026-07-10", d)$name, "passo-fundo_2026-07-10")
  expect_equal(resolve_site_config("passo-fundo_2026-07-10.yaml", d)$name, "passo-fundo_2026-07-10")
  # bare location: latest sowing date on or before today
  expect_equal(resolve_site_config("passo-fundo", d, today = as.Date("2026-07-15"))$name,
               "passo-fundo_2026-07-10")
  expect_equal(resolve_site_config("passo-fundo", d, today = as.Date("2026-07-10"))$name,
               "passo-fundo_2026-07-10")
  expect_error(resolve_site_config("passo-fundo", d, today = as.Date("2026-01-01")), "after")
  expect_error(resolve_site_config("nowhere", d), "Available")
})

test_that("layering: locations table, file name date, base values kept", {
  root <- make_project()
  put(root, "passo-fundo_2026-06-20.yaml", c("crop:", "  cultivar: Test cv"))
  f <- file.path(root, "configs", "passo-fundo_2026-06-20.yaml")
  m <- read_site_config(f, file.path(root, "base.yaml"), file.path(root, "locations.yaml"))
  expect_equal(m$cfg$location$name, "Passo Fundo")
  expect_equal(m$cfg$location$latitude, -28.262)
  expect_equal(m$cfg$crop$sowing_date, "2026-06-20")
  expect_equal(m$cfg$crop$cultivar, "Test cv")
  expect_equal(m$cfg$crop$cultivar_susceptibility, 0.7)       # from base
  expect_equal(m$cfg$pathogen$infection_min_wet_hours, 8)     # from base
  expect_equal(unlist(m$cfg$soil$FC), rep(0.30, 4))           # from locations
})

test_that("a site file can override the location soil and base settings", {
  root <- make_project()
  put(root, "passo-fundo_2026-06-20.yaml",
      c("soil:", "  FC: [0.4, 0.4, 0.4, 0.4]", "pathogen:", "  infection_min_wet_hours: 10"))
  m <- read_site_config(file.path(root, "configs", "passo-fundo_2026-06-20.yaml"),
                        file.path(root, "base.yaml"), file.path(root, "locations.yaml"))
  expect_equal(unlist(m$cfg$soil$FC), rep(0.4, 4))
  expect_equal(m$cfg$pathogen$infection_min_wet_hours, 10)
})

test_that("contents that disagree with the file name are refused", {
  root <- make_project(); b <- file.path(root, "base.yaml"); l <- file.path(root, "locations.yaml")
  put(root, "passo-fundo_2026-06-20.yaml", c("crop:", "  sowing_date: 2026-06-21"))
  expect_error(read_site_config(file.path(root, "configs", "passo-fundo_2026-06-20.yaml"), b, l),
               "disagrees")
  put(root, "passo-fundo_2026-06-22.yaml", "location: londrina")
  expect_error(read_site_config(file.path(root, "configs", "passo-fundo_2026-06-22.yaml"), b, l),
               "disagrees")
  put(root, "londrina_2026-06-20.yaml", "crop: {}")
  expect_error(read_site_config(file.path(root, "configs", "londrina_2026-06-20.yaml"), b, l),
               "not listed")
  expect_error(read_site_config(file.path(root, "configs", "londrina_2026-06-20.yaml"), b, NULL),
               "locations.yaml")
})

test_that("load_site_configuration gives the usual loader result", {
  root <- make_project()
  put(root, "passo-fundo_2026-06-20.yaml", c("crop:", "  cultivar_susceptibility: 0.5"))
  res <- load_site_configuration(file.path(root, "configs", "passo-fundo_2026-06-20.yaml"),
                                 file.path(root, "base.yaml"), file.path(root, "locations.yaml"))
  expect_equal(res$config$start_date, as.Date("2026-06-20"))
  expect_equal(res$config$latitude, -28.262)
  expect_equal(res$config$location_name, "Passo Fundo")
  expect_equal(res$cultivar_susceptibility, 0.5)
  expect_equal(res$soil$FC, rep(0.30, 4))
  expect_equal(res$config_name, "passo-fundo_2026-06-20")
  expect_true(res$config$nasa_power_open_meteo)
})

test_that("cultivars.yaml: entry sits between the base and the site file", {
  root <- make_project(); b <- file.path(root, "base.yaml")
  l <- file.path(root, "locations.yaml"); cv <- file.path(root, "cultivars.yaml")
  writeLines(c("early:", "  crop:", "    cultivar: Early one", "    cultivar_susceptibility: 0.4",
               "    TT_Anthesis: 1200", "  pathogen:", "    spike_heading_peak_TT: 1200"), cv)
  put(root, "passo-fundo_2026-06-20.yaml", c("crop:", "  cultivar: early"))
  m <- read_site_config(file.path(root, "configs", "passo-fundo_2026-06-20.yaml"), b, l, cv)
  expect_equal(m$cfg$crop$cultivar, "Early one")               # display name
  expect_equal(m$cfg$crop$cultivar_susceptibility, 0.4)        # entry beats base (0.7)
  expect_equal(m$cfg$crop$TT_Anthesis, 1200)
  expect_equal(m$cfg$pathogen$spike_heading_peak_TT, 1200)
  expect_equal(m$cfg$pathogen$infection_min_wet_hours, 8)      # base untouched

  # the site file's own values beat the entry
  put(root, "passo-fundo_2026-06-22.yaml",
      c("crop:", "  cultivar: early", "  TT_Anthesis: 1250"))
  m2 <- read_site_config(file.path(root, "configs", "passo-fundo_2026-06-22.yaml"), b, l, cv)
  expect_equal(m2$cfg$crop$TT_Anthesis, 1250)
  expect_equal(m2$cfg$crop$cultivar_susceptibility, 0.4)
  expect_equal(m2$cfg$crop$cultivar, "Early one")

  res <- load_site_configuration(file.path(root, "configs", "passo-fundo_2026-06-20.yaml"), b, l, cv)
  expect_equal(res$params$TT_Anthesis, 1200)
  expect_equal(res$cultivar, "Early one")
  expect_equal(res$cultivar_susceptibility, 0.4)
})

test_that("plain cultivar labels still work; unknown ids warn; no cultivars file is fine", {
  root <- make_project(); b <- file.path(root, "base.yaml"); l <- file.path(root, "locations.yaml")
  cv <- file.path(root, "cultivars.yaml")
  writeLines(c("early:", "  crop:", "    TT_Anthesis: 1200"), cv)
  put(root, "passo-fundo_2026-06-20.yaml", c("crop:", "  cultivar: My Local Cultivar"))
  f <- file.path(root, "configs", "passo-fundo_2026-06-20.yaml")
  expect_no_warning(m <- read_site_config(f, b, l, cv))
  expect_equal(m$cfg$crop$cultivar, "My Local Cultivar")
  expect_equal(m$cfg$crop$TT_Anthesis, NULL)

  put(root, "passo-fundo_2026-06-21.yaml", c("crop:", "  cultivar: earlyy"))
  expect_warning(read_site_config(file.path(root, "configs", "passo-fundo_2026-06-21.yaml"), b, l, cv),
                 "not in cultivars.yaml")
  # no cultivars file at all: the id is just a label, no warning
  expect_no_warning(read_site_config(file.path(root, "configs", "passo-fundo_2026-06-21.yaml"), b, l, NULL))
})

test_that("list_cultivars and read_cultivar_entry serve the dashboard dropdown", {
  skip_if_not_installed("yaml")
  cv <- tempfile(fileext = ".yaml")
  writeLines(c("early:", "  crop:", "    cultivar: Early one", "    TT_Anthesis: 1200",
               "  pathogen:", "    spike_heading_peak_TT: 1200",
               "plain:", "  crop:", "    TT_Anthesis: 1300"), cv)
  ch <- list_cultivars(cv)
  expect_equal(unname(ch), c("early", "plain"))
  expect_equal(names(ch), c("Early one (early)", "plain"))
  e <- read_cultivar_entry(cv, "early")
  expect_equal(e$crop$cultivar, "Early one")
  expect_equal(e$pathogen$spike_heading_peak_TT, 1200)
  expect_equal(read_cultivar_entry(cv, "plain")$crop$cultivar, "plain")
  expect_null(read_cultivar_entry(cv, "missing"))
  expect_equal(list_cultivars(NULL), character())
  expect_equal(list_cultivars(tempfile()), character())
})

test_that("the shipped locations, cultivars and configs are consistent", {
  skip_if_not_installed("yaml")
  # yaml::read_yaml() stops on duplicate keys, so this also catches a doubled entry
  loc <- yaml::read_yaml(file.path(project_root, "locations.yaml"))
  expect_false(anyDuplicated(names(loc)) > 0)
  cult <- yaml::read_yaml(file.path(project_root, "cultivars.yaml"))
  tab <- list_site_configs(file.path(project_root, "configs"))
  expect_gt(nrow(tab), 0)
  expect_false(anyDuplicated(tab$name) > 0)
  expect_true(all(tab$location %in% names(loc)))
  for (i in seq_len(nrow(tab))) {
    m <- read_site_config(tab$file[i], file.path(project_root, "wheat_blast_model_config.yaml"),
                          file.path(project_root, "locations.yaml"),
                          file.path(project_root, "cultivars.yaml"))
    expect_true(is.finite(as.numeric(m$cfg$location$latitude)), info = tab$name[i])
  }
})
