# ============================================================================
# 07_phenology.R
# Phenological stage dates from thermal time.
# ============================================================================

# --- 6. Phenology -----------------------------------------------------------

calculate_phenological_stages <- function(output_df, params, start_date) {
  stages <- c("Emergence", "DoubleRidge", "TerminalSpikelet",
              "FlagLeaf", "Anthesis", "InitialKernelFormation",
              "HardDough", "PhysiologicalMaturity")
  thresholds <- c(params$TT_Emergence, params$TT_DoubleRidge,
                  params$TT_TerminalSpikelet, params$TT_FlagLeaf,
                  params$TT_Anthesis, params$blast_kernel_start_TT,
                  params$blast_hard_dough_TT, params$TTM)
  stage_time <- sapply(thresholds, function(threshold) {
    idx <- which(output_df$Thermal_Time >= threshold)
    if (length(idx)) output_df$Time[idx[1]] else NA_real_
  })
  result <- data.frame(
    Stage = stages, TT_Threshold = thresholds,
    Date = start_date + stage_time - 1
  )
  result[!is.na(result$Date), ]
}
