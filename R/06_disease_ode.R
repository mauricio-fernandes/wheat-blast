# ============================================================================
# 06_disease_ode.R
# Crop/disease ODE, initial state and the solver wrapper.
# ============================================================================

# --- 4. ODE model -----------------------------------------------------------

create_interpolation_functions <- function(soil_water_data) {
  required <- c("tday", "topsoil_theta", "p", "srad", "tmean",
                "tmax", "tmin", "rh_wet_hours", "root_zone_swc")
  if (!all(required %in% names(soil_water_data))) {
    stop("Soil water data must contain: ", paste(required, collapse = ", "),
         call. = FALSE)
  }
  dat <- soil_water_data[complete.cases(soil_water_data[, required]), ]
  if (nrow(dat) < 2) stop("At least two complete rows are required.",
                          call. = FALSE)
  make_interp <- function(y) approxfun(dat$tday, y, rule = 2)
  list(
    M_interp_function = make_interp(dat$topsoil_theta),
    Rain_interp_function = make_interp(dat$p),
    Srad_interp_function = make_interp(dat$srad),
    TempMean_interp_function = make_interp(dat$tmean),
    Tmax_interp_function = make_interp(dat$tmax),
    Tmin_interp_function = make_interp(dat$tmin),
    # Hours of RH >= threshold belong to a whole day, so they are held constant
    # from the start of the day (t = d .. d + 1) instead of being interpolated.
    RHWetHours_step_function = approxfun(
      dat$tday, dat$rh_wet_hours, method = "constant", f = 0, rule = 2
    ),
    WaterStress_interp_function = make_interp(dat$root_zone_swc)
  )
}

spore_dynamics_model_ode_improved <- function(t, state, parameters) {
  with(as.list(state), with(as.list(parameters), {
    M_t <- interp_funcs$M_interp_function(t)
    Rain_t <- max(0, interp_funcs$Rain_interp_function(t))
    Srad_t <- max(0, interp_funcs$Srad_interp_function(t))
    Tmean_t <- interp_funcs$TempMean_interp_function(t)
    Tmax_t <- interp_funcs$Tmax_interp_function(t)
    Tmin_t <- interp_funcs$Tmin_interp_function(t)
    # Infection needs BOTH a favorable temperature (cardinal response on the
    # daily mean temperature) AND a wet period: at least infection_min_wet_hours
    # hours of the day with RH at or above the wet threshold.
    RHWetHours_t <- interp_funcs$RHWetHours_step_function(t)
    wet_period_gate <- as.numeric(RHWetHours_t >= infection_min_wet_hours)
    infection_temp_factor <- cardinal_temperature_response(
      Tmean_t, leaf_Tmin, leaf_Topt, leaf_Tmax
    )
    RootZoneSWC_t <- max(0, interp_funcs$WaterStress_interp_function(t))

    crop_active <- t >= crop_sowing_time
    cold_temperature_range <- max(
      1e-6,
      blast_cold_damage_threshold_C - blast_cold_full_damage_C
    )
    cold_damage_factor <- clamp01(
      (blast_cold_damage_threshold_C - Tmin_t) /
        cold_temperature_range
    )
    fungal_viability <- clamp01(Blast_Sporulation_Viability)
    dBlast_Sporulation_Viabilitydt <-
      -blast_cold_damage_rate * cold_damage_factor *
        fungal_viability +
      blast_cold_recovery_rate * (1 - cold_damage_factor) *
        (1 - fungal_viability)
    if (
      Blast_Sporulation_Viability +
        dBlast_Sporulation_Viabilitydt * time_step_size < 0
    ) {
      dBlast_Sporulation_Viabilitydt <-
        -Blast_Sporulation_Viability / time_step_size
    }
    if (
      Blast_Sporulation_Viability +
        dBlast_Sporulation_Viabilitydt * time_step_size > 1
    ) {
      dBlast_Sporulation_Viabilitydt <-
        (1 - Blast_Sporulation_Viability) / time_step_size
    }

    dTTdt <- if (crop_active) max(0, Tmean_t - Tbase) else 0
    water_stress_factor <- water_stress_function(RootZoneSWC_t)
    if (!crop_active) {
      dLAIdt <- 0
      dBdt <- 0
    } else if (TT < TT_Anthesis) {
      establishment_lai <- max(LAI, 0.01)
      dLAIdt <- ALPHA * establishment_lai *
        (1 - establishment_lai / LAIMAX) * water_stress_factor
    } else {
      dLAIdt <- -LAI_SenescenceRate * LAI
    }
    if (LAI + dLAIdt * time_step_size < 0) {
      dLAIdt <- -LAI / time_step_size
    }
    if (crop_active) {
      intercepted_srad <- Srad_t * (1 - exp(-K * LAI))
      dBdt <- RUE * intercepted_srad * water_stress_factor
    }
    if (TT > TTM) {
      dLAIdt <- 0
      dBdt <- 0
    }

    diseased_leaf_area <- max(0, Leaf_Latent + Leaf_Infectious +
                               Leaf_Necrotic)
    leaf_disease_capacity <- max(0, LAI) * clamp01(leaf_max_diseased_fraction)
    leaf_occupied_fraction <- if (leaf_disease_capacity > 0) {
      clamp01(diseased_leaf_area / leaf_disease_capacity)
    } else 0
    healthy_leaf_area <- max(0, leaf_disease_capacity - diseased_leaf_area)
    healthy_leaf_fraction <- if (leaf_disease_capacity > 0) {
      clamp01(healthy_leaf_area / leaf_disease_capacity)
    } else 0
    # Crowding progressively suppresses additional lesions before the
    # maximum foliar lesion-bearing fraction is reached.
    leaf_crowding_factor <- (1 - leaf_occupied_fraction) ^
      max(1, leaf_infection_crowding_exponent)

    if (isTRUE(leaf_disease_enabled) && crop_active) {
      inoculum_duration <- max(leaf_primary_inoculum_duration,
                               time_step_size)
      primary_inoculum_rate <- if (
        is.finite(leaf_initial_inoculum_time) &&
          t >= leaf_initial_inoculum_time &&
          t < leaf_initial_inoculum_time + inoculum_duration
      ) leaf_initial_inoculum_size / inoculum_duration else 0

      secondary_spore_production <- leaf_spore_production_rate *
        Leaf_Infectious * infection_temp_factor *
        wet_period_gate * fungal_viability
      effective_spores <- leaf_spore_infection_efficiency *
        Leaf_Spores * infection_temp_factor * wet_period_gate
      new_leaf_infection <- leaf_infection_rate * effective_spores *
        leaf_max_lesion_size * healthy_leaf_fraction * leaf_crowding_factor
      new_leaf_infection <- min(new_leaf_infection,
                                healthy_leaf_area / time_step_size)
      spores_causing_infection <- min(
        Leaf_Spores / time_step_size,
        leaf_infection_rate * effective_spores
      )
      latent_to_infectious <- Leaf_Latent /
        max(leaf_latent_period, time_step_size)
      infectious_to_necrotic <- Leaf_Infectious /
        max(leaf_infectious_period, time_step_size)

      dLeaf_Latentdt <- new_leaf_infection - latent_to_infectious
      dLeaf_Infectiousdt <- latent_to_infectious -
        infectious_to_necrotic
      dLeaf_Necroticdt <- infectious_to_necrotic
      # Residue-borne spores deposited on foliage and locally retained
      # secondary spores provide inoculum for new foliar lesions.
      dLeaf_Sporesdt <- primary_inoculum_rate +
        blast_leaf_deposition_rate * Blast_Airborne +
        (1 - blast_foliar_airborne_fraction) *
          secondary_spore_production -
        leaf_spore_mortality_rate * Leaf_Spores -
        spores_causing_infection

      leaf_senescence_rate <- max(0, -dLAIdt)
      if (LAI > 0 && diseased_leaf_area > 0 &&
          leaf_senescence_rate > 0) {
        dLeaf_Latentdt <- dLeaf_Latentdt -
          leaf_senescence_rate * Leaf_Latent / LAI
        dLeaf_Infectiousdt <- dLeaf_Infectiousdt -
          leaf_senescence_rate * Leaf_Infectious / LAI
        dLeaf_Necroticdt <- dLeaf_Necroticdt -
          leaf_senescence_rate * Leaf_Necrotic / LAI
      }
      if (Leaf_Latent + dLeaf_Latentdt * time_step_size < 0)
        dLeaf_Latentdt <- -Leaf_Latent / time_step_size
      if (Leaf_Infectious + dLeaf_Infectiousdt * time_step_size < 0)
        dLeaf_Infectiousdt <- -Leaf_Infectious / time_step_size
      if (Leaf_Necrotic + dLeaf_Necroticdt * time_step_size < 0)
        dLeaf_Necroticdt <- -Leaf_Necrotic / time_step_size
      if (Leaf_Spores + dLeaf_Sporesdt * time_step_size < 0)
        dLeaf_Sporesdt <- -Leaf_Spores / time_step_size
    } else {
      dLeaf_Latentdt <- dLeaf_Infectiousdt <- dLeaf_Necroticdt <- 0
      dLeaf_Sporesdt <- 0
      secondary_spore_production <- 0
    }

    # Alternative-host infection cycle. The host is present before and after
    # wheat sowing and uses the same infection, latency, infectious-period,
    # temperature, moisture, sporulation, and spore-survival parameters.
    alt_host_lai <- max(0, alternative_host_lai)
    alt_diseased_area <- max(
      0, Alt_Latent + Alt_Infectious + Alt_Necrotic
    )
    alt_healthy_area <- max(0, alt_host_lai - alt_diseased_area)
    alt_healthy_fraction <- if (alt_host_lai > 0) {
      alt_healthy_area / alt_host_lai
    } else 0

    if (isTRUE(alternative_host_enabled) && alt_host_lai > 0) {
      alt_effective_spores <- leaf_spore_infection_efficiency *
        Alt_Spores * infection_temp_factor * wet_period_gate
      new_alt_infection <- leaf_infection_rate *
        alt_effective_spores * leaf_max_lesion_size *
        alt_healthy_fraction
      new_alt_infection <- min(
        new_alt_infection, alt_healthy_area / time_step_size
      )
      alt_spores_causing_infection <- min(
        Alt_Spores / time_step_size,
        leaf_infection_rate * alt_effective_spores
      )
      alt_latent_to_infectious <- Alt_Latent /
        max(leaf_latent_period, time_step_size)
      alt_infectious_to_necrotic <- Alt_Infectious /
        max(leaf_infectious_period, time_step_size)
      alternative_spore_production <-
        alternative_host_spore_production_multiplier *
        leaf_spore_production_rate * Alt_Infectious *
        infection_temp_factor * wet_period_gate * fungal_viability
      alternative_host_deposition <- blast_leaf_deposition_rate *
        Blast_Airborne * alt_host_lai

      dAlt_Latentdt <- new_alt_infection -
        alt_latent_to_infectious
      dAlt_Infectiousdt <- alt_latent_to_infectious -
        alt_infectious_to_necrotic
      dAlt_Necroticdt <- alt_infectious_to_necrotic
      dAlt_Sporesdt <- alternative_host_deposition -
        leaf_spore_mortality_rate * Alt_Spores -
        alt_spores_causing_infection

      if (Alt_Latent + dAlt_Latentdt * time_step_size < 0)
        dAlt_Latentdt <- -Alt_Latent / time_step_size
      if (Alt_Infectious + dAlt_Infectiousdt * time_step_size < 0)
        dAlt_Infectiousdt <- -Alt_Infectious / time_step_size
      if (Alt_Necrotic + dAlt_Necroticdt * time_step_size < 0)
        dAlt_Necroticdt <- -Alt_Necrotic / time_step_size
      if (Alt_Spores + dAlt_Sporesdt * time_step_size < 0)
        dAlt_Sporesdt <- -Alt_Spores / time_step_size
    } else {
      dAlt_Latentdt <- dAlt_Infectiousdt <- dAlt_Necroticdt <- 0
      dAlt_Sporesdt <- 0
      alternative_spore_production <- 0
      alternative_host_deposition <- 0
    }

    # Primary inoculum is produced in crop residues when the topsoil is wet.
    topsoil_rew <- clamp01(
      (M_t - topsoil_wp) / max(1e-6, topsoil_fc - topsoil_wp)
    )
    residue_moisture_factor <- clamp01(
      (topsoil_rew - blast_residue_moisture_threshold) /
        max(1e-6, 1 - blast_residue_moisture_threshold)
    )
    release_temp_factor <- cardinal_temperature_response(
      Tmean_t, blast_release_Tmin, blast_release_Topt,
      blast_release_Tmax
    )

    rain_optimum <- max(1e-6, blast_rain_optimum_mm)
    if (Rain_t <= rain_optimum) {
      rain_release_factor <- 1 +
        blast_light_rain_benefit_max * Rain_t / rain_optimum
      heavy_rain_excess <- 0
    } else {
      heavy_rain_excess <- Rain_t - rain_optimum
      rain_release_factor <-
        (1 + blast_light_rain_benefit_max) *
        exp(-blast_heavy_rain_release_suppression_per_mm *
              heavy_rain_excess)
    }
    # Daily first-order washout rate above the rainfall optimum.
    heavy_rain_washout_rate <-
      blast_air_washout_rate_per_mm * heavy_rain_excess

    residue_spore_release <- blast_residue_spore_rate *
      Blast_Residue_Inoculum * residue_moisture_factor *
      release_temp_factor * rain_release_factor * fungal_viability

    # Infectious foliar lesions create secondary spores. A configurable
    # fraction enters the airborne pool and can reach leaves or spikes.
    foliar_airborne_release <- if (isTRUE(leaf_disease_enabled)) {
      blast_foliar_airborne_fraction * secondary_spore_production
    } else 0
    # Spike infection is modeled separately from foliar lesions. It is
    # concentrated around heading/anthesis, when spikes are most at risk.
    # Calculate this before using it to route airborne spores to spikes.
    spike_heading_factor <- calculate_heading_susceptibility(
      TT, blast_spike_heading_start_TT, blast_spike_heading_peak_TT,
      blast_spike_heading_end_TT
    )
    leaf_deposition <- if (
      crop_active && isTRUE(leaf_disease_enabled)
    ) blast_leaf_deposition_rate * Blast_Airborne else 0
    spike_deposition <- if (crop_active) {
      blast_spike_deposition_rate * Blast_Airborne * spike_heading_factor
    } else 0

    dBlast_Residue_Inoculumdt <- -residue_spore_release
    dBlast_Airbornedt <- residue_spore_release +
      foliar_airborne_release + alternative_spore_production -
      blast_airborne_mortality_rate * Blast_Airborne -
      heavy_rain_washout_rate * Blast_Airborne -
      leaf_deposition - alternative_host_deposition -
      spike_deposition
    dBlast_Kernel_Sporesdt <- spike_deposition -
      blast_kernel_spore_mortality_rate * Blast_Kernel_Spores

    # Spike susceptibility is the heading-window function alone. The former
    # kernel-stage terms (kernel_susceptible, kernel_moisture_factor) never
    # changed the result: max(h, k * h) == h, and the 0.80 kernel moisture
    # threshold can never exceed the 0.65 spike threshold. blast_kernel_start_TT
    # and blast_hard_dough_TT are now phenology markers only.
    spike_susceptible <- spike_heading_factor
    # Spike infection needs the same two conditions as foliar infection.
    spike_infection_factor <- wet_period_gate * infection_temp_factor
    dBlast_Kernel_Infectiondt <- blast_kernel_infection_rate *
      Blast_Kernel_Spores * blast_spike_susceptibility *
      spike_susceptible * spike_infection_factor -
      blast_kernel_recovery_rate * Blast_Kernel_Infection

    if (Blast_Residue_Inoculum +
        dBlast_Residue_Inoculumdt * time_step_size < 0)
      dBlast_Residue_Inoculumdt <-
        -Blast_Residue_Inoculum / time_step_size
    airborne_spore_floor <- max(0, blast_airborne_spore_minimum)
    if (
      Blast_Airborne + dBlast_Airbornedt * time_step_size <
        airborne_spore_floor
    ) {
      dBlast_Airbornedt <-
        (airborne_spore_floor - Blast_Airborne) / time_step_size
    }
    if (Blast_Kernel_Spores +
        dBlast_Kernel_Sporesdt * time_step_size < 0)
      dBlast_Kernel_Sporesdt <- -Blast_Kernel_Spores / time_step_size
    if (Blast_Kernel_Infection +
        dBlast_Kernel_Infectiondt * time_step_size < 0)
      dBlast_Kernel_Infectiondt <-
        -Blast_Kernel_Infection / time_step_size

    list(c(
      dTTdt, dLAIdt, dBdt,
      dBlast_Residue_Inoculumdt,
      dBlast_Sporulation_Viabilitydt, dBlast_Airbornedt,
      dBlast_Kernel_Sporesdt, dBlast_Kernel_Infectiondt,
      dLeaf_Latentdt, dLeaf_Infectiousdt, dLeaf_Necroticdt,
      dLeaf_Sporesdt,
      dAlt_Latentdt, dAlt_Infectiousdt, dAlt_Necroticdt,
      dAlt_Sporesdt
    ))
  }))
}

# --- 5. Simulation helpers --------------------------------------------------

default_initial_state <- function(
    blast_initial_residue_inoculum = 100,
    blast_initial_airborne_spores = 100,
    alternative_host_initial_infectious_area = 0) {
  c(
    TT = 0, LAI = 0, B = 0,
    Blast_Residue_Inoculum = blast_initial_residue_inoculum,
    Blast_Sporulation_Viability = 1,
    Blast_Airborne = blast_initial_airborne_spores,
    Blast_Kernel_Spores = 0,
    Blast_Kernel_Infection = 0,
    Leaf_Latent = 0, Leaf_Infectious = 0, Leaf_Necrotic = 0,
    Leaf_Spores = 0,
    Alt_Latent = 0,
    Alt_Infectious = max(0, alternative_host_initial_infectious_area),
    Alt_Necrotic = 0,
    Alt_Spores = 0
  )
}

run_crop_disease_ode <- function(
    weather_data, soil_water_data, params, simulation_start_date,
    crop_sowing_date,
    time_step = 0.1, initial_state = default_initial_state(),
    leaf_initial_inoculum_date = NA,
    leaf_initial_inoculum_size = 0) {
  params$interp_funcs <- create_interpolation_functions(soil_water_data)
  params$crop_sowing_time <-
    as.numeric(crop_sowing_date - simulation_start_date) + 1
  params$leaf_initial_inoculum_time <-
    as.numeric(as.Date(leaf_initial_inoculum_date) -
                 simulation_start_date) + 1
  if (is.na(params$leaf_initial_inoculum_time)) {
    params$leaf_initial_inoculum_time <- Inf
  }
  params$leaf_initial_inoculum_size <- leaf_initial_inoculum_size
  default_state <- default_initial_state(
    alternative_host_initial_infectious_area =
      params$alternative_host_initial_infectious_area
  )
  missing_names <- setdiff(names(default_state), names(initial_state))
  if (length(missing_names)) {
    initial_state <- c(initial_state, default_state[missing_names])
  }
  initial_state <- initial_state[names(default_state)]
  initial_state["Blast_Airborne"] <- max(
    initial_state["Blast_Airborne"],
    params$blast_airborne_spore_minimum
  )
  time_points <- seq(1, nrow(weather_data), by = time_step)
  params$time_step_size <- time_points[2] - time_points[1]

  output_df <- as.data.frame(ode(
    y = initial_state, times = time_points,
    func = spore_dynamics_model_ode_improved, parms = params
  ))
  names(output_df) <- c(
    "Time", "Thermal_Time", "LAI", "Biomass",
    "Blast_Residue_Inoculum", "Blast_Sporulation_Viability",
    "Blast_Airborne_Spores",
    "Blast_Kernel_Spores", "Blast_Kernel_Infection",
    "Leaf_Latent", "Leaf_Infectious", "Leaf_Necrotic",
    "Leaf_Spores",
    "Alternative_Host_Latent", "Alternative_Host_Infectious",
    "Alternative_Host_Necrotic", "Alternative_Host_Spores"
  )
  # Remove negligible solver-rounding excursions below the biological floor.
  output_df$Blast_Airborne_Spores <- pmax(
    params$blast_airborne_spore_minimum,
    output_df$Blast_Airborne_Spores
  )
  output_df$Date <- simulation_start_date + output_df$Time - 1
  output_df$Tmin <- params$interp_funcs$Tmin_interp_function(output_df$Time)
  output_df$Tmean <- params$interp_funcs$TempMean_interp_function(output_df$Time)
  output_df$Heading_Susceptibility <- vapply(
    output_df$Thermal_Time,
    calculate_heading_susceptibility,
    numeric(1),
    start = params$blast_spike_heading_start_TT,
    peak = params$blast_spike_heading_peak_TT,
    end = params$blast_spike_heading_end_TT
  )
  # The two infection conditions, as the ODE evaluates them: hours per day with
  # RH at or above the wet threshold (held constant within each day), the
  # resulting wet-period gate (0/1) and the temperature response (0-1).
  output_df$RH_Wet_Hours <- params$interp_funcs$RHWetHours_step_function(output_df$Time)
  output_df$Wet_Period_Met <- as.numeric(
    output_df$RH_Wet_Hours >= params$infection_min_wet_hours
  )
  output_df$Infection_Temp_Factor <- vapply(
    output_df$Tmean, cardinal_temperature_response, numeric(1),
    tmin = params$leaf_Tmin, topt = params$leaf_Topt, tmax = params$leaf_Tmax
  )
  output_df$Infection_Favorability <-
    output_df$Wet_Period_Met * output_df$Infection_Temp_Factor
  output_df$Leaf_Diseased <- output_df$Leaf_Latent +
    output_df$Leaf_Infectious + output_df$Leaf_Necrotic
  output_df$Leaf_Healthy <- pmax(0, output_df$LAI -
                                  output_df$Leaf_Diseased)
  output_df$Leaf_Disease_Severity <- ifelse(
    output_df$LAI > 0,
    pmin(1, output_df$Leaf_Diseased / output_df$LAI), 0
  )
  output_df$Leaf_Disease_Severity_Pct <-
    output_df$Leaf_Disease_Severity * 100
  output_df$Leaf_Lesion_Cap <- output_df$LAI * params$leaf_max_diseased_fraction
  output_df$Leaf_Lesion_Coverage_Pct <- ifelse(
    output_df$Leaf_Lesion_Cap > 0,
    100 * pmin(1, output_df$Leaf_Diseased / output_df$Leaf_Lesion_Cap), 0
  )
  output_df$Leaf_Disease_AUDPC <- calculate_audpc(
    output_df$Time, output_df$Leaf_Disease_Severity_Pct
  )
  output_df$Alternative_Host_Diseased <-
    output_df$Alternative_Host_Latent +
    output_df$Alternative_Host_Infectious +
    output_df$Alternative_Host_Necrotic
  output_df$Alternative_Host_Healthy <- pmax(
    0, params$alternative_host_lai -
      output_df$Alternative_Host_Diseased
  )
  output_df$Alternative_Host_Severity <- if (
    params$alternative_host_lai > 0
  ) {
    pmin(
      1, output_df$Alternative_Host_Diseased /
        params$alternative_host_lai
    )
  } else 0
  output_df$Alternative_Host_Severity_Pct <-
    output_df$Alternative_Host_Severity * 100
  output_df$Alternative_Host_AUDPC <- calculate_audpc(
    output_df$Time,
    output_df$Alternative_Host_Severity_Pct
  )
  output_df
}
