get_survey_adapter <- function(adapter_name) {
  adapter_map <- list(
    cmap_phase1 = standardize_cmap_phase1,
    cmap_2018_2019 = standardize_cmap_2018_2019,
    dvrpc_public = standardize_dvrpc_public,
    chicago_2024_2025 = standardize_cmap_phase1,
    chicago_2018_2019 = standardize_cmap_2018_2019,
    philly_dvrpc = standardize_dvrpc_public
  )

  if (!adapter_name %in% names(adapter_map)) {
    stop(paste0("Unknown survey adapter: ", adapter_name), call. = FALSE)
  }

  adapter_map[[adapter_name]]
}

make_mode_group <- function(label) {
  label_std <- tolower(as.character(label))

  dplyr::case_when(
    is.na(label_std) ~ NA_character_,
    stringr::str_detect(label_std, "walk") ~ "walk",
    stringr::str_detect(label_std, "bike|bicycle|divvy|micromobility|scooter") ~ "bike_micromobility",
    stringr::str_detect(label_std, "school bus") ~ "school_bus",
    stringr::str_detect(label_std, "bus|rail|train|cta|metra|pace|transit|subway|streetcar|tram|ferry|paratransit|dial-a-ride|call-n-ride") ~ "public_transit",
    stringr::str_detect(label_std, "carpool|hov|vanpool") ~ "carpool",
    stringr::str_detect(label_std, "taxi|tnc|uber|lyft|limo") ~ "taxi_tnc",
    stringr::str_detect(label_std, "shuttle") ~ "shuttle",
    stringr::str_detect(label_std, "auto|car|truck|van|sov|driver|passenger|motorcycle|moped|private vehicle") ~ "private_auto",
    TRUE ~ "other"
  )
}

make_purpose_group <- function(label) {
  label_std <- tolower(as.character(label))

  dplyr::case_when(
    is.na(label_std) ~ NA_character_,
    stringr::str_detect(label_std, "home") ~ "home",
    stringr::str_detect(label_std, "work|volunteer") ~ "work",
    stringr::str_detect(label_std, "school|class|daycare|education") ~ "school",
    stringr::str_detect(label_std, "escort|pick|drop|accompany") ~ "escort",
    stringr::str_detect(label_std, "shop|shopping|grocery|gas") ~ "shopping",
    stringr::str_detect(label_std, "meal|dined|coffee|restaurant|eat out|take-out|dining") ~ "meal",
    stringr::str_detect(label_std, "social|recreation|religious|community|entertainment|exercise|gym|event|visit") ~ "social_recreation",
    stringr::str_detect(label_std, "errand|personal business|bank|post office|government|medical|health care|appointment|library|salon") ~ "errand_personal_business",
    stringr::str_detect(label_std, "change mode|transfer") ~ "change_mode",
    stringr::str_detect(label_std, "overnight") ~ "overnight",
    stringr::str_detect(label_std, "loop") ~ "loop",
    TRUE ~ "other"
  )
}

assign_time_bins <- function(trips_df, scenario_time_bin_rules) {
  if (nrow(trips_df) == 0) {
    trips_df$time_bin <- character()
    return(trips_df)
  }

  trips_df <- trips_df %>% mutate(time_bin = NA_character_)

  if (is.null(scenario_time_bin_rules) || length(scenario_time_bin_rules) == 0) {
    return(trips_df %>% mutate(time_bin = "all_day"))
  }

  for (rule in scenario_time_bin_rules) {
    start_sec <- hms_to_seconds(rule$start_time)
    end_sec <- hms_to_seconds(rule$end_time)
    in_window <- !is.na(trips_df$depart_seconds) & trips_df$depart_seconds >= start_sec & trips_df$depart_seconds < end_sec
    trips_df$time_bin[in_window & is.na(trips_df$time_bin)] <- as.character(rule$time_bin)
  }

  trips_df
}

make_income_group_simple <- function(income_raw) {
  x <- safe_numeric(income_raw)
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x <= 2 ~ "low",
    x <= 4 ~ "lower_mid",
    x <= 6 ~ "upper_mid",
    x >= 7 ~ "high",
    TRUE ~ "other"
  )
}

make_vehicle_group_simple <- function(vehicles) {
  x <- safe_integer(vehicles)
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x <= 0 ~ "zero_vehicle",
    x == 1 ~ "one_vehicle",
    x >= 2 ~ "two_plus_vehicle",
    TRUE ~ NA_character_
  )
}

save_standardized_survey <- function(std_list, out_dir) {
  fs::dir_create(out_dir)

  write_csv_gz(std_list$households, file.path(out_dir, "households_std.csv.gz"))
  write_csv_gz(std_list$persons, file.path(out_dir, "persons_std.csv.gz"))

  if (!is.null(std_list$days)) {
    write_csv_gz(std_list$days, file.path(out_dir, "days_std.csv.gz"))
  }

  write_csv_gz(std_list$trips, file.path(out_dir, "trips_std.csv.gz"))

  if (!is.null(std_list$stages)) {
    write_csv_gz(std_list$stages, file.path(out_dir, "stages_std.csv.gz"))
  }

  if (!is.null(std_list$qa_summary)) {
    write_csv_gz(std_list$qa_summary, file.path(out_dir, "qa_summary.csv.gz"))
  }

  write_json_pretty(std_list$metadata, file.path(out_dir, "survey_metadata.json"))
  invisible(out_dir)
}
