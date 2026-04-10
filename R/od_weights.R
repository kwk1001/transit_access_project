read_standardized_survey <- function(cfg) {
  households <- read_csv_guess(file.path(cfg$paths$survey_dir, "households_std.csv.gz")) %>%
    standardize_tract_id_cols(c("home_tract")) %>%
    mutate(
      household_id = as.character(household_id)
    )

  persons <- read_csv_guess(file.path(cfg$paths$survey_dir, "persons_std.csv.gz")) %>%
    standardize_tract_id_cols(c("work_tract", "school_tract")) %>%
    mutate(
      household_id = as.character(household_id),
      person_id = as.character(person_id)
    )

  trips <- read_csv_guess(file.path(cfg$paths$survey_dir, "trips_std.csv.gz")) %>%
    standardize_tract_id_cols(c("origin_tract", "destination_tract")) %>%
    mutate(
      trip_id = as.character(trip_id),
      household_id = as.character(household_id),
      person_id = as.character(person_id),
      day_id = as.character(day_id),
      travel_dow = safe_integer(travel_dow),
      weekday_flag = coerce_flag(weekday_flag),
      complete_day_flag = coerce_flag(complete_day_flag),
      complete_trip_flag = coerce_flag(complete_trip_flag),
      transit_involved_flag = coerce_flag(transit_involved_flag),
      analysis_weight = safe_numeric(analysis_weight),
      duration_minutes = safe_numeric(duration_minutes),
      distance_miles = safe_numeric(distance_miles)
    )

  list(
    households = households,
    persons = persons,
    trips = trips
  )
}

study_area_county_fips <- function(cfg) {
  options(tigris_use_cache = TRUE)
  purrr::map_chr(cfg$analysis_area$counties, function(x) {
    state <- as.character(x$state)
    county_name <- as.character(x$county)
    county_tbl <- tigris::counties(state = state, cb = TRUE, year = cfg$analysis_area$county_outline_year, class = "sf", progress_bar = FALSE)
    row_i <- county_tbl %>% dplyr::filter(NAME == county_name) %>% dplyr::slice(1)
    if (nrow(row_i) == 0) {
      stop(paste0("County not found in tigris lookup: ", county_name, ", ", state), call. = FALSE)
    }
    as.character(row_i$GEOID[[1]])
  })
}

apply_trip_filters <- function(trips_df, scenario, cfg) {
  out <- trips_df %>%
    mutate(
      analysis_weight = safe_numeric(analysis_weight),
      duration_minutes = safe_numeric(duration_minutes),
      distance_miles = safe_numeric(distance_miles),
      travel_dow = safe_integer(travel_dow),
      weekday_flag = coerce_flag(weekday_flag),
      transit_involved_flag = coerce_flag(transit_involved_flag),
      origin_tract = standardize_geoid11(origin_tract),
      destination_tract = standardize_geoid11(destination_tract)
    )

  out <- assign_time_bins(out, scenario$time_bin_rules)

  if (!is.null(scenario$time_bin_rules) && length(scenario$time_bin_rules) > 0) {
    out <- out %>% filter(!is.na(time_bin))
  }

  if (isTRUE(scenario$weekday_only)) {
    out <- out %>% filter(weekday_flag)
  }

  allowed_dow <- safe_integer(unlist(scenario$allowed_travel_dow))
  if (length(allowed_dow) > 0 && any(!is.na(allowed_dow))) {
    out <- out %>% filter(travel_dow %in% allowed_dow)
  }

  if (isTRUE(scenario$transit_only)) {
    out <- out %>% filter(transit_involved_flag)
  }

  allowed_purpose <- unlist(scenario$allowed_purpose_groups)
  if (length(allowed_purpose) > 0) {
    out <- out %>% filter(purpose_group %in% allowed_purpose)
  }

  allowed_modes <- unlist(scenario$allowed_mode_groups)
  if (length(allowed_modes) > 0) {
    out <- out %>% filter(mode_group %in% allowed_modes)
  }

  if (isTRUE(scenario$exclude_same_tract)) {
    out <- out %>% filter(origin_tract != destination_tract)
  }

  county_keep <- study_area_county_fips(cfg)
  out %>%
    filter(
      !is.na(origin_tract),
      !is.na(destination_tract),
      !is.na(analysis_weight),
      analysis_weight > 0,
      stringr::str_sub(origin_tract, 1, 5) %in% county_keep,
      stringr::str_sub(destination_tract, 1, 5) %in% county_keep
    )
}

apply_auxiliary_od_multipliers <- function(od_df, cfg) {
  out <- od_df %>% mutate(origin_multiplier = 1, destination_multiplier = 1)
  aux_cfg <- cfg$auxiliary_weights

  if (!is.null(aux_cfg$origin_multiplier_file) && !is.na(aux_cfg$origin_multiplier_file) && file.exists(aux_cfg$origin_multiplier_file)) {
    origin_aux <- read_csv_guess(aux_cfg$origin_multiplier_file) %>%
      transmute(origin_id = standardize_geoid11(.data[[aux_cfg$origin_id_col]]), origin_multiplier = safe_numeric(.data[[aux_cfg$origin_multiplier_col]]))

    out <- out %>%
      left_join(origin_aux, by = c("origin_id" = "origin_id"), suffix = c("", "_aux")) %>%
      mutate(origin_multiplier = dplyr::coalesce(origin_multiplier_aux, origin_multiplier)) %>%
      select(-origin_multiplier_aux)
  }

  if (!is.null(aux_cfg$destination_multiplier_file) && !is.na(aux_cfg$destination_multiplier_file) && file.exists(aux_cfg$destination_multiplier_file)) {
    dest_aux <- read_csv_guess(aux_cfg$destination_multiplier_file) %>%
      transmute(destination_id = standardize_geoid11(.data[[aux_cfg$destination_id_col]]), destination_multiplier = safe_numeric(.data[[aux_cfg$destination_multiplier_col]]))

    out <- out %>%
      left_join(dest_aux, by = c("destination_id" = "destination_id"), suffix = c("", "_aux")) %>%
      mutate(destination_multiplier = dplyr::coalesce(destination_multiplier_aux, destination_multiplier)) %>%
      select(-destination_multiplier_aux)
  }

  out %>% mutate(weight_sum_adjusted = weight_sum * origin_multiplier * destination_multiplier)
}

prune_od_table <- function(od, cfg) {
  minimum_weight <- cfg$od_settings$minimum_weight %||% 0
  minimum_share <- cfg$od_settings$minimum_weight_share_within_origin %||% 0
  max_dest_per_origin <- cfg$od_settings$max_destinations_per_origin %||% Inf

  od %>%
    group_by(scenario_id, origin_id) %>%
    mutate(
      origin_total_weight_raw = sum(weight_sum, na.rm = TRUE),
      weight_share_within_origin = if_else(origin_total_weight_raw > 0, weight_sum / origin_total_weight_raw, NA_real_),
      rank_within_origin = min_rank(desc(weight_sum))
    ) %>%
    ungroup() %>%
    filter(
      weight_sum >= minimum_weight,
      dplyr::coalesce(weight_share_within_origin, 0) >= minimum_share,
      rank_within_origin <= max_dest_per_origin
    )
}

build_one_od_scenario <- function(trips_df, scenario, cfg) {
  filtered <- apply_trip_filters(trips_df, scenario, cfg)

  od <- filtered %>%
    group_by(origin_id = origin_tract, destination_id = destination_tract, time_bin) %>%
    summarise(
      n_trips_raw = n(),
      weight_sum = sum(analysis_weight, na.rm = TRUE),
      avg_distance_miles = weighted_mean_safe(distance_miles, analysis_weight),
      avg_duration_minutes = weighted_mean_safe(duration_minutes, analysis_weight),
      income_group_simple = first_non_missing(income_group_simple),
      vehicle_group_simple = first_non_missing(vehicle_group_simple),
      .groups = "drop"
    ) %>%
    mutate(
      origin_id = standardize_geoid11(origin_id),
      destination_id = standardize_geoid11(destination_id),
      time_bin = as.character(time_bin),
      scenario_id = as.character(scenario$scenario_id),
      scenario_label = as.character(scenario$scenario_label),
      source_id = cfg$active_survey_source_id
    )

  od <- prune_od_table(od, cfg)
  od <- apply_auxiliary_od_multipliers(od, cfg)

  origin_marginals <- od %>%
    group_by(source_id, scenario_id, origin_id) %>%
    summarise(origin_total_weight = sum(weight_sum_adjusted, na.rm = TRUE), .groups = "drop")

  dest_marginals <- od %>%
    group_by(source_id, scenario_id, destination_id) %>%
    summarise(destination_total_weight = sum(weight_sum_adjusted, na.rm = TRUE), .groups = "drop")

  od <- od %>%
    left_join(origin_marginals, by = c("source_id", "scenario_id", "origin_id")) %>%
    left_join(dest_marginals, by = c("source_id", "scenario_id", "destination_id"))

  top_n <- cfg$map$od_top_pairs_max %||% 500

  list(
    od = od,
    origin_marginals = origin_marginals,
    destination_marginals = dest_marginals,
    top_pairs = od %>% arrange(desc(weight_sum_adjusted)) %>% slice_head(n = top_n)
  )
}

build_all_od_weights <- function(cfg) {
  survey_std <- read_standardized_survey(cfg)
  trips_std <- survey_std$trips

  scenario_results <- purrr::map(cfg$od_scenarios, ~ build_one_od_scenario(trips_std, .x, cfg))

  od_all <- purrr::map_dfr(scenario_results, "od") %>% standardize_tract_id_cols(c("origin_id", "destination_id"))
  origin_all <- purrr::map_dfr(scenario_results, "origin_marginals") %>% standardize_tract_id_cols(c("origin_id"))
  dest_all <- purrr::map_dfr(scenario_results, "destination_marginals") %>% standardize_tract_id_cols(c("destination_id"))
  top_pairs_all <- purrr::map_dfr(scenario_results, "top_pairs") %>% standardize_tract_id_cols(c("origin_id", "destination_id"))

  fs::dir_create(cfg$paths$od_dir)
  write_csv_gz(od_all, file.path(cfg$paths$od_dir, "od_weights_all.csv.gz"))
  readr::write_csv(origin_all, file.path(cfg$paths$od_dir, "od_marginals_origin.csv"))
  readr::write_csv(dest_all, file.path(cfg$paths$od_dir, "od_marginals_destination.csv"))
  readr::write_csv(top_pairs_all, file.path(cfg$paths$od_dir, "top_od_pairs.csv"))

  invisible(list(od = od_all, origin = origin_all, destination = dest_all, top_pairs = top_pairs_all))
}

read_od_weights <- function(cfg) {
  read_csv_guess(file.path(cfg$paths$od_dir, "od_weights_all.csv.gz")) %>%
    standardize_tract_id_cols(c("origin_id", "destination_id")) %>%
    mutate(
      scenario_id = as.character(scenario_id),
      time_bin = as.character(time_bin)
    )
}
