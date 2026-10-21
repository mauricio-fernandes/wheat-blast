# ============================================================================
# 01_utils.R
# Small shared helpers: unit conversions, clamp01, %||%.
# ============================================================================

mm_to_m3m3 <- function(mm, dz) mm / (dz * 1000)
m3m3_to_mm <- function(theta, dz) theta * dz * 1000
clamp01 <- function(x) pmax(0, pmin(1, x))
`%||%` <- function(x, y) if (is.null(x)) y else x
