read_dvrpc_public_zip <- function(zip_path, tmp_dir) {
  utils::unzip(zip_path, exdir = tmp_dir, overwrite = TRUE)
  tmp_dir
}

standardize_dvrpc_public <- function(cfg) {
  tmp_dir <- tempfile("dvrpc_public_")
  fs::dir_create(tmp_dir)
  on.exit(fs::dir_delete(tmp_dir), add = TRUE)

  data_dir <- read_dvrpc_public_zip(cfg$active_survey_source$file_path, tmp_dir)

  hh_path <- file.path(data_dir, "DVRPC HTS Database Files", "1_Household_Public.xlsx")
  person_path <- file.path(data_dir, "DVRPC HTS Database Files", "2_Person_Public.xlsx")
  trip_path <- file.path(data_dir, "DVRPC HTS Database Files", "4_Trip_Public.xlsx")

  hh <- readxl::read_excel(hh_path) %>% janitor::clean_names()
  person <- readxl::read_excel(person_path) %>% janitor::clean_names()
  trip <- readxl::read_excel(trip_path) %>% janitor::clean_names()

  households_std <- hh %>%
    transmute(
      city_id = cfg$project$city_id,
      survey_name = cfg$project$survey_name,
      survey_wave = cfg$project$survey_wave,
      household_id = as.character(hh_id),
      household_weight = safe_numeric(hh_weight),
      home_tract = standardize_geoid11(h_tract),
      home_county_fips = stringr::str_sub(home_tract, 1, 5),
      home_state_fips = stringr::str_sub(home_tract, 1, 2),
      income_code_raw = as.character(income),
      income_group_raw = as.character(income),
      income_group_simple = make_income_group_simple(income),
      vehicles = safe_integer(tot_veh),
      vehicle_group_simple = make_vehicle_group_simple(tot_veh),
      persons = safe_integer(hh_size),
      workers = safe_integer(hh_work),
      students = NA_integer_,
      survey_start_date = suppressWarnings(as.Date(trav_date, format = "%m/%d/%Y")),
      survey_end_date = suppressWarnings(as.Date(trav_date, format = "%m/%d/%Y")),
      complete_flag = !is.na(safe_numeric(hh_weight)),
      raw_source_file = "1_Household_Public.xlsx"
    )

  persons_std <- person %>%
    transmute(
      city_id = cfg$project$city_id,
      survey_name = cfg$project$survey_name,
      survey_wave = cfg$project$survey_wave,
      household_id = as.character(hh_id),
      person_id = as.character(person_id),
      person_weight = safe_numeric(p_weight),
      age = safe_integer(agecat),
      gender_code_raw = as.character(gend),
      worker_flag = safe_integer(emply) == 1,
      student_flag = safe_integer(stude) %in% c(1, 2),
      work_tract = standardize_geoid11(w_tract),
      school_tract = standardize_geoid11(s_tract),
      person_type_raw = as.character(relat),
      complete_flag = !is.na(safe_numeric(p_weight)),
      raw_source_file = "2_Person_Public.xlsx"
    ) %>%
    left_join(
      households_std %>%
        select(household_id, household_weight, income_group_simple, vehicle_group_simple),
      by = "household_id"
    )

  household_day_context <- hh %>%
    transmute(
      household_id = as.character(hh_id),
      hh_travel_date = suppressWarnings(as.Date(trav_date, format = "%m/%d/%Y")),
      hh_travel_dow = safe_integer(trav_dow)
    )

  days_std <- hh %>%
    transmute(
      city_id = cfg$project$city_id,
      survey_name = cfg$project$survey_name,
      survey_wave = cfg$project$survey_wave,
      household_id = as.character(hh_id),
      person_id = NA_character_,
      day_id = paste0(as.character(hh_id), "_", as.character(trav_date)),
      travel_date = suppressWarnings(as.Date(trav_date, format = "%m/%d/%Y")),
      travel_dow = safe_integer(trav_dow),
      weekday_flag = safe_integer(trav_dow) %in% c(2, 3, 4, 5, 6),
      day_weight = safe_numeric(hh_weight),
      complete_day_flag = !is.na(safe_numeric(hh_weight)),
      raw_source_file = "1_Household_Public.xlsx"
    )

  trip_seq <- trip %>%
    mutate(
      household_id = as.character(hh_id),
      person_id = as.character(person_id),
      trip_num = safe_integer(trip_num)
    ) %>%
    arrange(person_id, trip_num)

  activity_to_label <- function(code_chr) {
    code_chr <- as.character(code_chr)
    dplyr::case_when(
      code_chr == "1" ~ "Home",
      code_chr %in% c("2", "3", "4") ~ "School",
      code_chr == "5" ~ "Work",
      code_chr %in% c("6", "7", "16") ~ "Errand",
      code_chr %in% c("8", "9", "10") ~ "Shop",
      code_chr == "11" ~ "Meal",
      code_chr %in% c("12", "13", "14", "15") ~ "Social/recreational",
      code_chr %in% c("17", "18", "20") ~ "Escort",
      code_chr == "19" ~ "Change mode",
      TRUE ~ "Other"
    )
  }

  trip_with_dest_purpose <- trip_seq %>%
    group_by(person_id) %>%
    mutate(
      destination_purpose_code_raw = dplyr::lead(as.character(activ1)),
      destination_purpose_label = activity_to_label(destination_purpose_code_raw)
    ) %>%
    ungroup()

  trips_std <- trip_with_dest_purpose %>%
    filter(!trip_num %in% c(0L, 97L)) %>%
    left_join(household_day_context, by = "household_id") %>%
    left_join(
      persons_std %>%
        select(
          person_id, household_id, household_weight, person_weight,
          income_group_simple, vehicle_group_simple, complete_flag
        ),
      by = c("person_id", "household_id")
    ) %>%
    mutate(
      travel_date = hh_travel_date,
      travel_dow = hh_travel_dow,
      weekday_flag = hh_travel_dow %in% c(2, 3, 4, 5, 6),
      depart_seconds = hhmm_to_seconds(depart),
      depart_minutes_of_day = time_to_minutes_of_day(depart_seconds),
      mode_group = dplyr::case_when(
        safe_integer(mode_agg) == 1 ~ "walk",
        safe_integer(mode_agg) == 2 ~ "bike_micromobility",
        safe_integer(mode_agg) == 3 ~ "private_auto",
        safe_integer(mode_agg) == 4 ~ "shuttle",
        safe_integer(mode_agg) == 5 ~ "public_transit",
        safe_integer(mode_agg) == 6 ~ "school_bus",
        TRUE ~ "other"
      ),
      transit_involved_flag = safe_integer(mode_agg) == 5,
      origin_purpose_code_raw = as.character(activ1),
      origin_purpose_label = activity_to_label(origin_purpose_code_raw),
      purpose_group = make_purpose_group(destination_purpose_label),
      complete_day_flag = TRUE,
      complete_trip_flag = complete_flag,
      trip_weight = dplyr::coalesce(
        safe_numeric(composite_weight),
        safe_numeric(p_weight) * safe_numeric(gps_factor),
        safe_numeric(p_weight)
      ),
      analysis_weight = trip_weight
    ) %>%
    transmute(
      city_id = cfg$project$city_id,
      survey_name = cfg$project$survey_name,
      survey_wave = cfg$project$survey_wave,
      trip_id = as.character(trip_id),
      household_id = household_id,
      person_id = person_id,
      day_id = paste0(household_id, "_", travel_date),
      travel_date = travel_date,
      travel_dow = travel_dow,
      travel_dow_label = as.character(travel_dow),
      weekday_flag = weekday_flag,
      complete_day_flag = complete_day_flag,
      complete_trip_flag = complete_trip_flag,
      origin_tract = standardize_geoid11(o_tract),
      destination_tract = standardize_geoid11(d_tract),
      origin_county_fips = stringr::str_sub(origin_tract, 1, 5),
      destination_county_fips = stringr::str_sub(destination_tract, 1, 5),
      depart_time_local = as.character(depart),
      arrive_time_local = as.character(arrive),
      depart_seconds = depart_seconds,
      depart_minutes_of_day = depart_minutes_of_day,
      duration_minutes = dplyr::coalesce(
        safe_numeric(survey_trav_time),
        safe_numeric(model_trav_time)
      ),
      distance_miles = safe_numeric(model_trav_dist),
      main_mode_code_raw = as.character(mode_agg),
      main_mode_label = as.character(mode_agg),
      mode_group = mode_group,
      transit_involved_flag = transit_involved_flag,
      origin_purpose_code_raw = origin_purpose_code_raw,
      origin_purpose_label = origin_purpose_label,
      destination_purpose_code_raw = destination_purpose_code_raw,
      destination_purpose_label = destination_purpose_label,
      purpose_category_code_raw = destination_purpose_code_raw,
      purpose_category_label = destination_purpose_label,
      purpose_group = purpose_group,
      household_weight = household_weight,
      person_weight = person_weight,
      day_weight = NA_real_,
      trip_weight = trip_weight,
      analysis_weight = analysis_weight,
      income_group_simple = income_group_simple,
      vehicle_group_simple = vehicle_group_simple,
      raw_source_file = "4_Trip_Public.xlsx",
      raw_record_id = as.character(record_id)
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
    adapter = "dvrpc_public",
    survey_zip = cfg$active_survey_source$file_path,
    files_used = c("1_Household_Public.xlsx", "2_Person_Public.xlsx", "4_Trip_Public.xlsx"),
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
