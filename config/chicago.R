project_config <- list(
  city_id = "chicago",
  city_label = "Chicago",
  study_area = list(
    state = "IL",
    counties = c("Cook", "DuPage", "Kane", "Kendall", "Lake", "McHenry", "Will"),
    tract_year = 2023,
    county_year = 2023
  ),
  survey_sources = list(
    list(
      source_id = "cmap_2024_2025_phase1",
      adapter = "cmap_phase1",
      file_path = "data/raw/surveys/chicago/MyDailyTravel_2024_2025Phase1.zip"
    ),
    list(
      source_id = "cmap_2018_2019",
      adapter = "cmap_2018_2019",
      file_path = "data/raw/surveys/chicago/MyDailyTravel_2018_2019.zip"
    )
  ),
  active_survey_source_id = "cmap_2024_2025_phase1",
  geography = list(
    service_buffer_m = 2000,
    restrict_to_gtfs_service_area = TRUE
  ),
  periods = data.frame(
    period_id = c("P1", "P2", "P3"),
    period_label = c(
      "2025-08-04_to_2025-08-25",
      "2025-08-25_to_2025-09-08",
      "2025-09-08_to_2025-09-29"
    ),
    start_date = c("2025-08-04", "2025-08-25", "2025-09-08"),
    end_date_exclusive = c("2025-08-25", "2025-09-08", "2025-09-29"),
    stringsAsFactors = FALSE
  ),
  weekday_only = TRUE,
  excluded_dates = c("2025-09-01", "2025-09-02"),
  time_windows = list(
    peak = list(start = "07:30:00", end = "09:30:00"),
    midday = list(start = "11:00:00", end = "14:00:00"),
    custom = list(start = "06:00:00", end = "22:00:00")
  ),
  od_weights = list(
    keep_only_internal_to_study_area = TRUE,
    weekday_only = TRUE,
    allowed_time_windows = c("peak", "midday", "custom"),
    transit_filter = "all",
    purpose_groups = NULL,
    minimum_weight = 0,
    minimum_weight_share_within_origin = 0,
    max_destinations_per_origin = 100,
    use_destination_population_multiplier = FALSE,
    destination_population_field = "estimate_total_population"
  ),
  gtfs = list(
    agency_name = "CTA",
    registry = data.frame(
      feed_name = c("CTA_0711", "CTA_0814"),
      file_path = c(
        "data/raw/gtfs/chicago/CTA-07-11.zip",
        "data/raw/gtfs/chicago/CTA-08-14.zip"
      ),
      valid_start = c("2025-07-08", "2025-08-12"),
      valid_end_exclusive = c("2025-08-12", "2025-10-31"),
      stringsAsFactors = FALSE
    )
  ),
  osm = list(
    pbf_url = "https://download.geofabrik.de/north-america/us/illinois-latest.osm.pbf",
    pbf_file = "data/raw/osm/chicago/illinois-latest.osm.pbf"
  ),
  routing = list(
    mode = c("WALK", "TRANSIT"),
    max_walk_time = 30,
    max_trip_duration = 180,
    walk_speed_kmh = 4.68,
    max_rides = 4,
    max_lts = 2,
    n_threads = 2,
    origin_batch_size = 40,
    java_memory = "12G",
    java_active_processors = 2
  ),
  map = list(
    comparison_metrics = c(
      "delta_avg_weighted_travel_time_penalized",
      "delta_pct_weighted_travel_time_penalized",
      "delta_unreachable_share_weighted",
      "share_weight_increase_gt_10",
      "share_weight_becomes_unreachable"
    ),
    period_metrics = c(
      "avg_weighted_travel_time_penalized",
      "share_unreachable_weighted",
      "share_accessible_45min_weighted"
    ),
    od_top_pairs_max = 500,
    od_top_pairs_default = 100
  ),
  optional_covariates = list(
    download_population = FALSE,
    census_api_key_env = "CENSUS_API_KEY",
    acs_year = 2023,
    acs_variables = c(total_population = "B01003_001"),
    tract_covariates_file = "data/processed/chicago/geography/tract_covariates.csv.gz"
  )
,
  run_label = NULL
)
