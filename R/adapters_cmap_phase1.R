read_cmap_phase1_zip <- function(zip_path, tmp_dir) {
  utils::unzip(zip_path, files = c("MyDailyTravelData.zip"), exdir = tmp_dir, overwrite = TRUE)
  inner_zip <- file.path(tmp_dir, "MyDailyTravelData.zip")
  inner_dir <- file.path(tmp_dir, "cmap_phase1_data")
  fs::dir_create(inner_dir)
  utils::unzip(inner_zip, exdir = inner_dir, overwrite = TRUE)
  inner_dir
}

read_cmap_phase1_value_labels <- function(data_dir) {
  codebook_path <- file.path(data_dir, "2024-2025 CMAP HTS Codebook.xlsx")
  readxl::read_excel(codebook_path, sheet = "value_labels") %>%
    janitor::clean_names()
}

make_cmap_phase1_lookup <- function(value_labels, variable_name) {
  value_labels %>%
    filter(variable == variable_name) %>%
    transmute(value = as.character(value), label = as.character(label))
}

standardize_cmap_phase1 <- function(cfg) {
  tmp_dir <- tempfile("cmap_phase1_")
  fs::dir_create(tmp_dir)
  on.exit(fs::dir_delete(tmp_dir), add = TRUE)

  data_dir <- read_cmap_phase1_zip(cfg$active_survey_source$file_path, tmp_dir)
  value_labels <- read_cmap_phase1_value_labels(data_dir)

  hh <- read_csv_raw_character(file.path(data_dir, "hh.csv"))
  person <- read_csv_raw_character(file.path(data_dir, "person.csv"))
  day <- read_csv_raw_character(file.path(data_dir, "day.csv"))
  trip <- read_csv_raw_character(file.path(data_dir, "trip_linked.csv"))

  mode_lookup <- make_cmap_phase1_lookup(value_labels, "linked_trip_mode")
  purpose_lookup <- make_cmap_phase1_lookup(value_labels, "d_purpose")
  purpose_cat_lookup <- make_cmap_phase1_lookup(value_labels, "d_purpose_category")
  dow_lookup <- make_cmap_phase1_lookup(value_labels, "travel_dow") %>%
    mutate(value = safe_integer(value))

  households_std <- hh %>%
    transmute(
      city_id = cfg$project$city_id,
      survey_name = cfg$project$survey_name,
      survey_wave = cfg$project$survey_wave,
      household_id = as.character(hh_id),
      household_weight = safe_numeric(hh_weight),
      home_tract = standardize_geoid11(home_tract_2020),
      home_county_fips = stringr::str_sub(home_tract, 1, 5),
      home_state_fips = stringr::str_sub(home_tract, 1, 2),
      income_code_raw = as.character(income_detailed),
      income_group_raw = as.character(income_broad),
      income_group_simple = make_income_group_simple(income_broad),
      vehicles = safe_integer(num_vehicles),
      vehicle_group_simple = make_vehicle_group_simple(num_vehicles),
      persons = safe_integer(num_people),
      workers = safe_integer(num_workers),
      students = safe_integer(num_students),
      survey_start_date = as.Date(first_travel_date),
      survey_end_date = as.Date(last_travel_date),
      complete_flag = dplyr::if_else(safe_integer(is_complete) == 1, TRUE, FALSE, missing = FALSE),
      raw_source_file = "hh.csv"
    )

  persons_std <- person %>%
    transmute(
      city_id = cfg$project$city_id,
      survey_name = cfg$project$survey_name,
      survey_wave = cfg$project$survey_wave,
      household_id = as.character(hh_id),
      person_id = as.character(person_id),
      person_weight = safe_numeric(person_weight),
      age = safe_integer(age),
      gender_code_raw = as.character(gender),
      worker_flag = safe_integer(employment) %in% c(1, 2, 3),
      student_flag = safe_integer(student) %in% c(1, 2),
      work_tract = standardize_geoid11(work_tract_2020),
      school_tract = standardize_geoid11(school_tract_2020),
      person_type_raw = as.character(person_type),
      complete_flag = dplyr::if_else(safe_integer(is_complete) == 1, TRUE, FALSE, missing = FALSE),
      raw_source_file = "person.csv"
    ) %>%
    left_join(
      households_std %>% select(household_id, household_weight, income_group_simple, vehicle_group_simple),
      by = "household_id"
    )

  days_std <- day %>%
    transmute(
      city_id = cfg$project$city_id,
      survey_name = cfg$project$survey_name,
      survey_wave = cfg$project$survey_wave,
      household_id = as.character(hh_id),
      person_id = as.character(person_id),
      day_id = as.character(day_id),
      travel_date = as.Date(travel_date),
      travel_dow = safe_integer(travel_dow),
      weekday_flag = safe_integer(travel_dow) %in% c(1L, 2L, 3L, 4L, 5L),
      day_weight = safe_numeric(day_weight),
      complete_day_flag = safe_integer(hh_day_complete) == 1 & safe_integer(summary_complete) == 1 & safe_integer(is_participant) == 1,
      raw_source_file = "day.csv"
    )

  trip_core <- trip %>%
    left_join(days_std %>% select(day_id, travel_date, travel_dow, weekday_flag, complete_day_flag, day_weight), by = "day_id") %>%
    left_join(persons_std %>% select(household_id, person_id, person_weight, household_weight, income_group_simple, vehicle_group_simple, complete_flag), by = c("hh_id" = "household_id", "person_id" = "person_id")) %>%
    left_join(mode_lookup, by = c("linked_trip_mode" = "value")) %>%
    rename(main_mode_label = label) %>%
    left_join(purpose_lookup, by = c("d_purpose" = "value")) %>%
    rename(destination_purpose_label = label) %>%
    left_join(purpose_cat_lookup, by = c("d_purpose_category" = "value")) %>%
    rename(purpose_category_label = label) %>%
    left_join(dow_lookup, by = c("travel_dow" = "value")) %>%
    rename(travel_dow_label = label)

  trips_std <- trip_core %>%
    mutate(
      depart_time_local = sprintf("%02d:%02d:%02d", safe_integer(depart_hour), safe_integer(depart_minute), dplyr::coalesce(safe_integer(depart_seconds), 0L)),
      arrive_time_local = sprintf("%02d:%02d:%02d", safe_integer(arrive_hour), safe_integer(arrive_minute), dplyr::coalesce(safe_integer(arrive_second), 0L)),
      depart_seconds = hms_to_seconds(depart_time_local),
      depart_minutes_of_day = time_to_minutes_of_day(depart_seconds),
      mode_group = make_mode_group(main_mode_label),
      transit_involved_flag = mode_group == "public_transit" | safe_integer(linked_trip_mode) %in% c(2, 3, 4, 5, 6, 7, 18),
      purpose_group = make_purpose_group(purpose_category_label),
      complete_trip_flag = TRUE,
      trip_weight = safe_numeric(linked_trip_weight),
      analysis_weight = dplyr::coalesce(trip_weight, day_weight, person_weight, household_weight)
    ) %>%
    transmute(
      city_id = cfg$project$city_id,
      survey_name = cfg$project$survey_name,
      survey_wave = cfg$project$survey_wave,
      trip_id = as.character(linked_trip_id),
      household_id = as.character(hh_id),
      person_id = as.character(person_id),
      day_id = as.character(day_id),
      travel_date = as.Date(travel_date),
      travel_dow = safe_integer(travel_dow),
      travel_dow_label = as.character(travel_dow_label),
      weekday_flag = weekday_flag,
      complete_day_flag = complete_day_flag,
      complete_trip_flag = complete_trip_flag,
      origin_tract = standardize_geoid11(o_tract_2020),
      destination_tract = standardize_geoid11(d_tract_2020),
      origin_county_fips = stringr::str_sub(origin_tract, 1, 5),
      destination_county_fips = stringr::str_sub(destination_tract, 1, 5),
      depart_time_local = depart_time_local,
      arrive_time_local = arrive_time_local,
      depart_seconds = depart_seconds,
      depart_minutes_of_day = depart_minutes_of_day,
      duration_minutes = safe_numeric(duration_minutes),
      distance_miles = safe_numeric(distance_miles),
      main_mode_code_raw = as.character(linked_trip_mode),
      main_mode_label = as.character(main_mode_label),
      mode_group = mode_group,
      transit_involved_flag = transit_involved_flag,
      origin_purpose_code_raw = as.character(o_purpose),
      origin_purpose_label = NA_character_,
      destination_purpose_code_raw = as.character(d_purpose),
      destination_purpose_label = as.character(destination_purpose_label),
      purpose_category_code_raw = as.character(d_purpose_category),
      purpose_category_label = as.character(purpose_category_label),
      purpose_group = purpose_group,
      household_weight = safe_numeric(household_weight),
      person_weight = safe_numeric(person_weight),
      day_weight = safe_numeric(day_weight),
      trip_weight = trip_weight,
      analysis_weight = analysis_weight,
      income_group_simple = income_group_simple,
      vehicle_group_simple = vehicle_group_simple,
      raw_source_file = "trip_linked.csv",
      raw_record_id = as.character(linked_trip_id)
    )

  households_std <- households_std %>% filter(complete_flag)
  persons_std <- persons_std %>% filter(complete_flag)
  days_std <- days_std %>% filter(complete_day_flag)
  trips_std <- trips_std %>%
    filter(complete_day_flag, complete_trip_flag) %>%
    semi_join(persons_std %>% select(person_id), by = "person_id") %>%
    semi_join(households_std %>% select(household_id), by = "household_id")

  qa_summary <- tibble(
    metric = c("n_households", "n_persons", "n_days", "n_trips", "n_trips_missing_origin", "n_trips_missing_destination", "n_trips_missing_weight"),
    value = c(
      nrow(households_std),
      nrow(persons_std),
      nrow(days_std),
      nrow(trips_std),
      sum(is.na(trips_std$origin_tract)),
      sum(is.na(trips_std$destination_tract)),
      sum(is.na(trips_std$analysis_weight))
    )
  )

  metadata <- list(
    adapter = "cmap_phase1",
    survey_zip = cfg$active_survey_source$file_path,
    files_used = c("hh.csv", "person.csv", "day.csv", "trip_linked.csv"),
    n_households = nrow(households_std),
    n_persons = nrow(persons_std),
    n_days = nrow(days_std),
    n_trips = nrow(trips_std)
  )

  list(
    households = households_std,
    persons = persons_std,
    days = days_std,
    trips = trips_std,
    stages = NULL,
    qa_summary = qa_summary,
    metadata = metadata
  )
}
