# ============================================================================
# 11_site_configs.R
# One small YAML file per location and sowing date, layered on the shared base
# configuration. File name = <location-slug>_<YYYY-MM-DD>.yaml, in ./configs.
#
#   wheat_blast_model_config.yaml   shared defaults (pathogen, infection rule, ...)
#   locations.yaml                  coordinates / time zone / soil per location slug
#   cultivars.yaml                  optional: thermal-time / susceptibility sets per cultivar id
#   configs/passo-fundo_2026-06-20.yaml   only what is specific to this run
#
# The file name is a selector; the contents are the source of truth. If the file
# also states a sowing date or a location slug, it must agree with the name.
# ============================================================================

# "Passo Fundo, RS" -> "passo-fundo" (text before the first comma, ASCII, lowercase)
slugify_location <- function(x) {
  x <- sub(",.*$", "", as.character(x %||% ""))
  # Work on Unicode code points so the result does not depend on the locale or on
  # iconv(//TRANSLIT), which differ between platforms (macOS gives "~a" for "a-tilde").
  from <- utf8ToInt(paste0(
    "\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u00e7\u00e8\u00e9\u00ea\u00eb",
    "\u00ec\u00ed\u00ee\u00ef\u00f1\u00f2\u00f3\u00f4\u00f5\u00f6",
    "\u00f9\u00fa\u00fb\u00fc\u00fd\u00ff",
    "\u00c0\u00c1\u00c2\u00c3\u00c4\u00c5\u00c7\u00c8\u00c9\u00ca\u00cb",
    "\u00cc\u00cd\u00ce\u00cf\u00d1\u00d2\u00d3\u00d4\u00d5\u00d6",
    "\u00d9\u00da\u00db\u00dc\u00dd"))
  to <- utf8ToInt("aaaaaaceeeeiiiinooooouuuuyyAAAAAACEEEEIIIINOOOOOUUUUY")
  codes <- utf8ToInt(enc2utf8(x))
  if (!length(codes)) return("")
  codes <- codes[!(codes >= 0x300 & codes <= 0x36F)]   # drop combining accents (a + ~)
  hit <- match(codes, from)
  codes[!is.na(hit)] <- to[hit[!is.na(hit)]]
  x <- intToUtf8(codes)
  x <- tolower(gsub("[^A-Za-z0-9]+", "-", x))
  gsub("^-+|-+$", "", x)
}

site_config_stem <- function(file) sub("\\.ya?ml$", "", basename(file), ignore.case = TRUE)

# <slug>_<date> -> list(location, sowing_date), or NULL if the name does not fit
parse_site_config_name <- function(file) {
  stem <- site_config_stem(file)
  m <- regmatches(stem, regexec(
    "^([a-z0-9]+(?:-[a-z0-9]+)*)_([0-9]{4}-[0-9]{2}-[0-9]{2})$", stem, perl = TRUE
  ))[[1]]
  if (length(m) != 3L) return(NULL)
  sowing <- as.Date(m[3], format = "%Y-%m-%d")
  if (is.na(sowing)) return(NULL)
  list(location = m[2], sowing_date = sowing)
}

# Table of the valid site configs in `dir`, sorted by location then sowing date.
list_site_configs <- function(dir) {
  empty <- data.frame(name = character(), location = character(),
                      sowing_date = as.Date(character()), file = character(),
                      stringsAsFactors = FALSE)
  if (!dir.exists(dir)) return(empty)
  files <- list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE, ignore.case = TRUE)
  rows <- lapply(files, function(f) {
    p <- parse_site_config_name(f)
    if (is.null(p)) return(NULL)
    data.frame(name = site_config_stem(f), location = p$location,
               sowing_date = p$sowing_date, file = f, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(empty)
  out <- do.call(rbind, rows)
  out[order(out$location, out$sowing_date), , drop = FALSE]
}

# selector = full name ("passo-fundo_2026-06-20") or a location slug ("passo-fundo").
# A bare location picks its most recent sowing date on or before `today`.
resolve_site_config <- function(selector, dir, today = Sys.Date()) {
  table <- list_site_configs(dir)
  if (!nrow(table)) stop("No site configurations (<location>_<YYYY-MM-DD>.yaml) in ", dir, call. = FALSE)
  selector <- site_config_stem(selector)
  exact <- table[table$name == selector, , drop = FALSE]
  if (nrow(exact)) return(exact[1, , drop = FALSE])
  rows <- table[table$location == selector, , drop = FALSE]
  if (!nrow(rows)) {
    stop("No configuration called '", selector, "'. Available: ",
         paste(table$name, collapse = ", "), call. = FALSE)
  }
  rows <- rows[rows$sowing_date <= as.Date(today), , drop = FALSE]
  if (!nrow(rows)) {
    stop("All configurations for '", selector, "' have a sowing date after ", today,
         ". Use the full name to run one anyway.", call. = FALSE)
  }
  rows[which.max(rows$sowing_date), , drop = FALSE]
}

# Base + locations + site file -> one merged YAML-shaped list (name and
# sowing date taken from, and checked against, the file name).
read_site_config <- function(file, base_path, locations_path = NULL, cultivars_path = NULL) {
  parsed <- parse_site_config_name(file)
  if (is.null(parsed)) {
    stop("Config file names must look like <location>_<YYYY-MM-DD>.yaml: ",
         basename(file), call. = FALSE)
  }
  base <- yaml::read_yaml(base_path)
  site <- yaml::read_yaml(file) %||% list()

  # location: a slug (string), an inline block, or absent (slug from the file name)
  loc <- site$location
  if (is.null(loc) || is.character(loc)) {
    slug <- loc %||% parsed$location
    if (!identical(slug, parsed$location)) {
      stop(basename(file), ": location '", slug, "' disagrees with the file name ('",
           parsed$location, "').", call. = FALSE)
    }
    if (is.null(locations_path) || !file.exists(locations_path)) {
      stop(basename(file), " uses the location '", slug, "' but locations.yaml was not found.",
           call. = FALSE)
    }
    entry <- (yaml::read_yaml(locations_path) %||% list())[[slug]]
    if (is.null(entry)) stop("Location '", slug, "' is not listed in locations.yaml.", call. = FALSE)
    site$location <- entry[intersect(c("name", "latitude", "longitude", "timezone"), names(entry))]
    if (is.null(site$soil) && !is.null(entry$soil)) site$soil <- entry$soil
  }

  # cultivar: crop.cultivar may be an id listed in cultivars.yaml. The entry's crop
  # and pathogen values sit between the base and the site file (site file wins);
  # crop.cultivar then becomes the entry's display name. Any other text is a plain label.
  id <- site$crop$cultivar
  if (is.character(id) && length(id) == 1L) {
    table <- if (!is.null(cultivars_path) && file.exists(cultivars_path)) {
      yaml::read_yaml(cultivars_path) %||% list()
    } else NULL
    entry <- table[[id]]
    if (!is.null(entry)) {
      from_entry <- entry[intersect(c("crop", "pathogen"), names(entry))]
      shown <- from_entry$crop$cultivar %||% id
      site <- utils::modifyList(from_entry, site)
      site$crop$cultivar <- shown
    } else if (!is.null(table) && grepl("^[a-z0-9]+(-[a-z0-9]+)*$", id)) {
      warning(basename(file), ": crop.cultivar '", id, "' looks like a cultivar id but is not in ",
              "cultivars.yaml; using it as a plain label with the base parameters.", call. = FALSE)
    }
  }

  # sowing date: from the name; if the file also states one it must agree
  stated <- site$crop$sowing_date
  if (!is.null(stated) && !identical(as.Date(as.character(stated)), parsed$sowing_date)) {
    stop(basename(file), ": crop.sowing_date (", stated, ") disagrees with the file name (",
         format(parsed$sowing_date), ").", call. = FALSE)
  }
  site$crop$sowing_date <- format(parsed$sowing_date)

  list(cfg = utils::modifyList(base, site), name = site_config_stem(file),
       location = parsed$location, sowing_date = parsed$sowing_date)
}

# Same return value as load_yaml_configuration(), plus $config_name.
load_site_configuration <- function(file, base_path, locations_path = NULL, cultivars_path = NULL) {
  merged <- read_site_config(file, base_path, locations_path, cultivars_path)
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(yaml::as.yaml(merged$cfg, handlers = list(logical = yaml::verbatim_logical)), tmp)
  loaded <- load_yaml_configuration(tmp)
  loaded$config_name <- merged$name
  loaded
}

# Named vector id -> label for a dropdown ("Display name (id)"); empty if no file.
list_cultivars <- function(path) {
  if (is.null(path) || !file.exists(path)) return(character())
  table <- yaml::read_yaml(path) %||% list()
  if (!length(table) || is.null(names(table))) return(character())
  ids <- names(table)
  shown <- vapply(table, function(e) as.character(e$crop$cultivar %||% NA_character_), character(1))
  stats::setNames(ids, ifelse(is.na(shown), ids, paste0(shown, " (", ids, ")")))
}

# The crop/pathogen values of one cultivar id (display name filled in), or NULL.
read_cultivar_entry <- function(path, id) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  entry <- (yaml::read_yaml(path) %||% list())[[id]]
  if (is.null(entry)) return(NULL)
  out <- entry[intersect(c("crop", "pathogen"), names(entry))]
  out$crop$cultivar <- entry$crop$cultivar %||% id
  out
}
