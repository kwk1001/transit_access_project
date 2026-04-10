read_cmap_2018_zip <- function(zip_path, tmp_dir) {
  utils::unzip(zip_path, files = c("MyDailyTravelData.zip"), exdir = tmp_dir, overwrite = TRUE)
  inner_zip <- file.path(tmp_dir, "MyDailyTravelData.zip")
  inner_dir <- file.path(tmp_dir, "cmap_2018_data")
  fs::dir_create(inner_dir)
  utils::unzip(inner_zip, exdir = inner_dir, overwrite = TRUE)
  inner_dir
}

read_cmap_2018_lookup <- function(data_dir) {
  readxl::read_excel(file.path(data_dir, "data_dictionary.xlsx"), sheet = "Value Lookup") %>%
    janitor::clean_names()
}

make_cmap_2018_value_lookup <- function(lookup_tbl, variable_name, table_name = NULL) {
  out <- lookup_tbl %>% filter(name == variable_name)
  if (!is.null(table_name)) {
    out <- out %>% filter(table == table_name)
  }
  out %>% transmute(value = as.character(value), label = as.character(label))
}

standardize_cmap_2018_2019 <- function(cfg) {
  tmp_dir <- tempfile("cmap_2018_")
  fs::dir_create(tmp_dir)
  on.exit(fs::dir_delete(tmp_dir), add = TRUE)

  data_dir <- read_cmap_2018_zip(cfg$active_survey_source$file_path, tmp_dir)
  lookup_tbl <- read_cmap_2018_lookup(data_dir)

  household <- read_csv_raw_character(file.path(data_dir, "household.csv"))
  person <- read_csv_raw_character(file.path(data_dir, "person.csv"))
  place <- read_csv_raw_character(file.path(data_dir, "place.csv"))
  location <- read_csv_raw_character(file.path(data_dir, "location.csv"))

  mode_lookup <- make_cmap_2018_value_lookup(lookup_tbl, "MODE", "PLACE")
  purpose_lookup <- make_cmap_2018_value_lookup(lookup_tbl, "TPURP", "PLACE")
  travday_lookup <- make_cmap_2018_value_lookup(lookup_tbl, "TRAVDAY", "HOUSEHOLD") %>%
    mutate(value = safe_integer(value))

  households_std <- household %>%
    transmute(
      city_id = cfg$project$city_id,
      survey_name = cfg$project$survey_name,
      survey_wave = cfg$project$survey_wave,
      household_id = as.character(sampno),
      household_weight = safe_numeric(wthhfin),
      income_code_raw = as.character(hhinc),
      income_group_raw = as.character(hhinc2),
      income_group_simple = make_income_group_simple(hhinc2),
      vehicles = safe_integer(hhveh),
      vehicle_group_simple = make_vehicle_group_simple(hhveh),
      persons = safe_integer(hhsize),
      workers = NA_integer_,
      students = NA_integer_,
      survey_start_date = as.Date(travdate),
      survey_end_date = as.Date(travdate),
      complete_flag = !is.na(safe_numeric(wthhfin)),
      raw_source_file = "household.csv"
    )

  home_location <- location %>%
    filter(safe_integer(home) == 1) %>%
    transmute(
      household_id = as.character(sampno),
      home_tract = build_geoid11_from_components(state_fips, county_fips, tract_fips),
      home_county_fips = paste0(zero_pad(state_fips, 2), zero_pad(county_fips, 3)),
      home_state_fips = zero_pad(state_fips, 2)
    ) %>%
    distinct(household_id, .keep_all = TRUE)

  households_std <- households_std %>% left_join(home_location, by = "household_id")

  persons_std <- person %>%
    transmute(
      city_id = cfg$project$city_id,
      survey_name = cfg$project$survey_name,
      survey_wave = cfg$project$survey_wave,
      household_id = as.character(sampno),
      person_id = paste0(as.character(sampno), "_", as.character(perno)),
      person_weight = safe_numeric(wtperfin),
      age = safe_integer(age),
      gender_code_raw = as.character(sex),
      worker_flag = safe_integer(wplace) == 1 | safe_integer(jobs) >= 1,
      student_flag = safe_integer(stude) %in% c(1, 2),
      work_tract = NA_character_,
      school_tract = NA_character_,
      person_type_raw = as.character(retmode_final),
      complete_flag = safe_integer(qc_trip_person) == 1 | !is.na(safe_numeric(wtperfin)),
      raw_source_file = "person.csv"
    ) %>%
    left_join(households_std %>% select(household_id, household_weight, income_group_simple, vehicle_group_simple), by = "household_id")

  days_std <- household %>%
    transmute(
      city_id = cfg$project$city_id,
      survey_name = cfg$project$survey_name,
      survey_wave = cfg$project$survey_wave,
      household_id = as.character(sampno),
      person_id = NA_character_,
      day_id = paste0(as.character(sampno), "_", as.character(travdate)),
      travel_date = as.Date(travdate),
      travel_dow = safe_integer(travday),
      weekday_flag = safe_integer(travday) %in% c(2, 3, 4, 5, 6),
      day_weight = safe_numeric(wthhfin),
      complete_day_flag = !is.na(as.Date(travdate)),
      raw_source_file = "household.csv"
    )

  location_key <- location %>%
    transmute(
      sampno = as.character(sampno),
      locno = as.character(locno),
      tract = build_geoid11_from_components(state_fips, county_fips, tract_fips),
      county_fips = paste0(zero_pad(state_fips, 2), zero_pad(county_fips, 3))
    )

  place_trips <- place %>%
    mutate(
      sampno = as.character(sampno),
      perno = as.character(perno),
      person_id = paste0(sampno, "_", perno),
      traveldayno = as.character(traveldayno),
      placeno_num = safe_integer(placeno),
      locno = as.character(locno)
    ) %>%
    arrange(sampno, perno, traveldayno, placeno_num) %>%
    group_by(sampno, perno, traveldayno) %>%
    mutate(
      origin_locno = lag(locno),
      origin_tpurp = lag(tpurp),
      origin_deptime = lag(deptime),
      trip_seq = placeno_num - 1L
    ) %>%
    ungroup() %>%
    filter(placeno_num > 1)

  trip_core <- place_trips %>%
    left_join(location_key %>% rename(origin_locno = locno, origin_tract = tract, origin_county_fips = county_fips), by = c("sampno", "origin_locno")) %>%
    left_join(location_key %>% rename(locno = locno, destination_tract = tract, destination_county_fips = county_fips), by = c("sampno", "locno")) %>%
    left_join(household %>% select(sampno, travdate, travday), by = "sampno") %>%
    left_join(persons_std %>% select(household_id, person_id, household_weight, person_weight, income_group_simple, vehicle_group_simple, complete_flag), by = c("sampno" = "household_id", "person_id" = "person_id")) %>%
    left_join(mode_lookup, by = c("mode" = "value")) %>%
    rename(main_mode_label = label) %>%
    left_join(purpose_lookup, by = c("tpurp" = "value")) %>%
    rename(destination_purpose_label = label) %>%
    left_join(purpose_lookup, by = c("origin_tpurp" = "value")) %>%
    rename(origin_purpose_label = label) %>%
    left_join(travday_lookup, by = c("travday" = "value")) %>%
    rename(travel_dow_label = label)

  trips_std <- trip_core %>%
    mutate(
      depart_seconds = hms_to_seconds(stringr::str_sub(origin_deptime, 12, 19)),
      depart_minutes_of_day = time_to_minutes_of_day(depart_seconds),
      mode_group = make_mode_group(main_mode_label),
      transit_involved_flag = mode_group == "public_transit" | safe_integer(mode) %in% c(500, 501, 502, 503, 504, 505, 506, 509),
      purpose_group = make_purpose_group(destination_purpose_label),
      weekday_flag = safe_integer(travday) %in% c(2, 3, 4, 5, 6),
      complete_day_flag = !is.na(as.Date(travdate)),
      complete_trip_flag = complete_flag,
      trip_weight = safe_numeric(person_weight),
      analysis_weight = dplyr::coalesce(trip_weight, household_weight)
    ) %>%
    transmute(
      city_id = cfg$project$city_id,
      survey_name = cfg$project$survey_name,
      survey_wave = cfg$project$survey_wave,
      trip_id = paste0(sampno, "_", perno, "_", traveldayno, "_", trip_seq),
      household_id = as.character(sampno),
      person_id = person_id,
      day_id = paste0(sampno, "_", travdate),
      travel_date = as.Date(travdate),
      travel_dow = safe_integer(travday),
      travel_dow_label = as.character(travel_dow_label),
      weekday_flag = weekday_flag,
      complete_day_flag = complete_day_flag,
      complete_trip_flag = complete_trip_flag,
      origin_tract = origin_tract,
      destination_tract = destination_tract,
      origin_county_fips = origin_county_fips,
      destination_county_fips = destination_county_fips,
      depart_time_local = stringr::str_sub(origin_deptime, 12, 19),
      arrive_time_local = stringr::str_sub(arrtime, 12, 19),
      depart_seconds = depart_seconds,
      depart_minutes_of_day = depart_minutes_of_day,
      duration_minutes = safe_numeric(travtime),
      distance_miles = safe_numeric(distance),
      main_mode_code_raw = as.character(mode),
      main_mode_label = as.character(main_mode_label),
      mode_group = mode_group,
      transit_involved_flag = transit_involved_flag,
      origin_purpose_code_raw = as.character(origin_tpurp),
      origin_purpose_label = as.character(origin_purpose_label),
      destination_purpose_code_raw = as.character(tpurp),
      destination_purpose_label = as.character(destination_purpose_label),
      purpose_category_code_raw = NA_character_,
      purpose_category_label = as.character(destination_purpose_label),
      purpose_group = purpose_group,
      household_weight = safe_numeric(household_weight),
      person_weight = safe_numeric(person_weight),
      day_weight = NA_real_,
      trip_weight = safe_numeric(trip_weight),
      analysis_weight = safe_numeric(analysis_weight),
      income_group_simple = income_group_simple,
      vehicle_group_simple = vehicle_group_simple,
      raw_source_file = "place.csv",
      raw_record_id = paste0(sampno, "_", perno, "_", traveldayno, "_", placeno)
    )

  households_std <- households_std %>% filter(complete_flag)
  persons_std <- persons_std %>% filter(complete_flag)
  days_std <- days_std %>% filter(complete_day_flag)
  trips_std <- trips_std %>%
    filter(complete_trip_flag) %>%
    semi_join(persons_std %>% select(person_id), by = "person_id") %>%
    semi_join(households_std %>% select(household_id), by = "household_id")

  qa_summary <- tibble(
    metric = c("n_households", "n_persons", "n_days", "n_trips"),
    value = c(nrow(households_std), nrow(persons_std), nrow(days_std), nrow(trips_std))
  )

  metadata <- list(
    adapter = "cmap_2018_2019",
    survey_zip = cfg$active_survey_source$file_path,
    files_used = c("household.csv", "person.csv", "place.csv", "location.csv"),
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
