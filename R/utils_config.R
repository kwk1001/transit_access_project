project_default_timezone <- function(city_id) {
  switch(
    city_id,
    chicago = "America/Chicago",
    philadelphia = "America/New_York",
    "America/New_York"
  )
}

make_county_specs_from_list_config <- function(cfg_raw) {
  county_tbl <- tibble(state = cfg_raw$study_area$state, county = unlist(cfg_raw$study_area$counties))
  split(county_tbl, seq_len(nrow(county_tbl))) %>%
    purrr::map(~ as.list(.x[1, ]))
}

make_survey_sources_from_yaml <- function(cfg_yaml) {
  survey_cfg <- cfg_yaml$survey %||% list()

  if (!is.null(cfg_yaml$survey_sources) && length(cfg_yaml$survey_sources) > 0) {
    return(purrr::map(cfg_yaml$survey_sources, function(x) {
      list(
        source_id = x$source_id %||% paste0(cfg_yaml$project$city_id, "_", cfg_yaml$project$survey_wave),
        adapter = x$adapter,
        file_path = x$file_path,
        label = x$label %||% x$source_id,
        keep_complete_only = isTRUE(x$keep_complete_only %||% survey_cfg$keep_complete_only)
      )
    }))
  }

  list(list(
    source_id = survey_cfg$source_id %||% paste0(cfg_yaml$project$city_id, "_", cfg_yaml$project$survey_wave),
    adapter = survey_cfg$adapter,
    file_path = survey_cfg$zip_path,
    label = survey_cfg$label %||% cfg_yaml$project$survey_name,
    keep_complete_only = isTRUE(survey_cfg$keep_complete_only)
  ))
}

make_survey_sources_from_list <- function(cfg_raw) {
  purrr::map(cfg_raw$survey_sources, function(x) {
    list(
      source_id = x$source_id,
      adapter = x$adapter,
      file_path = x$file_path
    )
  })
}

make_periods_from_yaml <- function(cfg_yaml) {
  dplyr::bind_rows(cfg_yaml$study_periods) %>%
    mutate(
      period_start = as.Date(period_start),
      period_end_exclusive = if ("period_end_exclusive" %in% names(.)) as.Date(period_end_exclusive) else as.Date(period_end) + 1
    ) %>%
    select(period_id, period_label, period_start, period_end_exclusive)
}

make_gtfs_feeds_from_yaml <- function(cfg_yaml) {
  purrr::map(cfg_yaml$gtfs_feeds, function(feed) {
    list(
      feed_name = feed$feed_name,
      valid_start = as.Date(feed$valid_start),
      valid_end_exclusive = as.Date(if (!is.null(feed$valid_end_exclusive)) feed$valid_end_exclusive else as.Date(feed$valid_end) + 1),
      gtfs_files = unlist(feed$gtfs_files)
    )
  })
}

make_od_scenarios_from_list <- function(cfg_raw) {
  purrr::imap(cfg_raw$time_windows, function(win, nm) {
    list(
      scenario_id = nm,
      scenario_label = paste(nm, "all purposes"),
      weekday_only = isTRUE(cfg_raw$od_weights$weekday_only),
      allowed_travel_dow = list(),
      transit_only = identical(cfg_raw$od_weights$transit_filter, "transit_only"),
      allowed_purpose_groups = cfg_raw$od_weights$purpose_groups,
      allowed_mode_groups = NULL,
      exclude_same_tract = TRUE,
      time_bin_rules = list(list(time_bin = nm, start_time = win$start, end_time = win$end))
    )
  })
}

make_routing_windows_from_list <- function(cfg_raw) {
  purrr::imap(cfg_raw$time_windows, function(win, nm) {
    list(
      time_window_id = nm,
      time_window_label = nm,
      od_scenario_id = nm,
      start_time = win$start,
      end_time = win$end
    )
  })
}

make_gtfs_feeds_from_list <- function(cfg_raw) {
  reg <- dplyr::bind_rows(cfg_raw$gtfs$registry)
  split(reg, seq_len(nrow(reg))) %>%
    purrr::map(function(x) {
      list(
        feed_name = x$feed_name[[1]],
        valid_start = as.Date(x$valid_start[[1]]),
        valid_end_exclusive = as.Date(x$valid_end_exclusive[[1]]),
        gtfs_files = c(x$file_path[[1]])
      )
    })
}

make_run_options <- function(run_options) {
  list(
    force = isTRUE(run_options$force),
    force_downloads = isTRUE(run_options$force_downloads),
    force_standardize = isTRUE(run_options$force_standardize),
    force_od = isTRUE(run_options$force_od),
    force_gtfs = isTRUE(run_options$force_gtfs),
    force_routing = isTRUE(run_options$force_routing),
    force_metrics = isTRUE(run_options$force_metrics),
    force_maps = isTRUE(run_options$force_maps)
  )
}

validate_normalized_config <- function(cfg) {
  required_period_cols <- c("period_id", "period_label", "period_start", "period_end_exclusive")
  survey_ids <- purrr::map_chr(cfg$survey_sources, "source_id")
  if (length(survey_ids) == 0) {
    stop("Config error: at least one survey source must be defined.", call. = FALSE)
  }
  if (anyDuplicated(survey_ids)) {
    stop("Config error: survey source ids must be unique.", call. = FALSE)
  }

  if (!is.data.frame(cfg$study_periods) || !all(required_period_cols %in% names(cfg$study_periods))) {
    stop("Config error: study_periods must contain period_id, period_label, period_start, and period_end_exclusive.", call. = FALSE)
  }

  required_od_fields <- c(
    "scenario_id", "scenario_label", "weekday_only", "allowed_travel_dow", "transit_only",
    "allowed_purpose_groups", "allowed_mode_groups", "exclude_same_tract", "time_bin_rules"
  )
  if (!is.list(cfg$od_scenarios) || length(cfg$od_scenarios) == 0) {
    stop("Config error: od_scenarios must be a non-empty list.", call. = FALSE)
  }
  purrr::iwalk(cfg$od_scenarios, function(sc, nm) {
    missing <- setdiff(required_od_fields, names(sc))
    if (length(missing) > 0) {
      stop(paste0("Config error in od_scenarios/", nm %||% "item", ": missing fields ", paste(missing, collapse = ", ")), call. = FALSE)
    }
    if (!is.list(sc$time_bin_rules) || length(sc$time_bin_rules) == 0) {
      stop(paste0("Config error in od_scenarios/", sc$scenario_id, ": time_bin_rules must be a non-empty list."), call. = FALSE)
    }
    purrr::walk(sc$time_bin_rules, function(rule) {
      needed <- c("time_bin", "start_time", "end_time")
      if (!all(needed %in% names(rule))) {
        stop(paste0("Config error in od_scenarios/", sc$scenario_id, ": each time_bin_rule must contain time_bin, start_time, end_time."), call. = FALSE)
      }
    })
  })

  required_routing_fields <- c("time_window_id", "time_window_label", "od_scenario_id", "start_time", "end_time")
  if (!is.list(cfg$routing$routing_windows) || length(cfg$routing$routing_windows) == 0) {
    stop("Config error: routing_windows must be a non-empty list.", call. = FALSE)
  }
  purrr::walk(cfg$routing$routing_windows, function(win) {
    if (!all(required_routing_fields %in% names(win))) {
      stop("Config error: each routing window must contain time_window_id, time_window_label, od_scenario_id, start_time, end_time.", call. = FALSE)
    }
  })

  od_ids <- purrr::map_chr(cfg$od_scenarios, "scenario_id")
  routing_ids <- purrr::map_chr(cfg$routing$routing_windows, "od_scenario_id")
  unknown_ids <- setdiff(routing_ids, od_ids)
  if (length(unknown_ids) > 0) {
    stop(paste0("Config error: routing_windows refer to unknown od_scenario_id values: ", paste(unknown_ids, collapse = ", ")), call. = FALSE)
  }

  cfg
}

normalize_yaml_config <- function(cfg_yaml, config_path) {
  city_id <- cfg_yaml$project$city_id
  survey_sources <- make_survey_sources_from_yaml(cfg_yaml)
  survey_cfg <- cfg_yaml$survey %||% list()
  active_survey_source_id <- survey_cfg$source_id %||% survey_cfg$default_source_id %||% survey_sources[[1]]$source_id

  list(
    project = list(
      city_id = city_id,
      city_label = cfg_yaml$project$city_label,
      timezone = cfg_yaml$project$timezone %||% project_default_timezone(city_id),
      survey_name = cfg_yaml$project$survey_name,
      survey_wave = cfg_yaml$project$survey_wave,
      config_path = config_path
    ),
    survey_sources = survey_sources,
    active_survey_source_id = active_survey_source_id,
    analysis_area = list(
      counties = cfg_yaml$analysis_area$counties,
      tract_year = cfg_yaml$analysis_area$tract_year,
      county_outline_year = cfg_yaml$analysis_area$county_outline_year %||% cfg_yaml$analysis_area$tract_year,
      zip_zcta_year = as.integer(cfg_yaml$analysis_area$zip_zcta_year %||% 2020),
      taz_file = cfg_yaml$analysis_area$taz_file %||% NULL,
      taz_id_col = cfg_yaml$analysis_area$taz_id_col %||% "taz_id",
      taz_name_col = cfg_yaml$analysis_area$taz_name_col %||% NULL
    ),
    geography = list(
      analysis_unit = normalize_analysis_unit(cfg_yaml$geography$analysis_unit %||% "tract"),
      service_buffer_m = cfg_yaml$routing$service_area_buffer_m %||% 2000,
      restrict_to_gtfs_service_area = isTRUE(cfg_yaml$routing$restrict_to_service_area)
    ),
    study_periods = make_periods_from_yaml(cfg_yaml),
    analysis_calendar = list(
      weekday_only = isTRUE(cfg_yaml$analysis_calendar$weekday_only),
      excluded_dates = if (is.null(cfg_yaml$analysis_calendar$excluded_dates)) as.Date(character()) else as.Date(unlist(cfg_yaml$analysis_calendar$excluded_dates))
    ),
    od_scenarios = cfg_yaml$od_scenarios,
    od_settings = list(
      minimum_weight = cfg_yaml$od_settings$minimum_weight %||% 0,
      minimum_weight_share_within_origin = cfg_yaml$od_settings$minimum_weight_share_within_origin %||% 0,
      max_destinations_per_origin = cfg_yaml$od_settings$max_destinations_per_origin %||% Inf
    ),
    synthetic_survey = list(
      enabled = isTRUE(cfg_yaml$synthetic_survey$enabled),
      max_travel_minutes = cfg_yaml$synthetic_survey$max_travel_minutes %||% 180,
      include_same_origin_destination = isTRUE(cfg_yaml$synthetic_survey$include_same_origin_destination)
    ),
    auxiliary_weights = cfg_yaml$auxiliary_weights %||% list(),
    gtfs_feeds = make_gtfs_feeds_from_yaml(cfg_yaml),
    osm = list(
      download_url = cfg_yaml$osm$download_url,
      local_pbf_path = cfg_yaml$osm$local_pbf_path
    ),
    routing = list(
      modes = cfg_yaml$routing$modes,
      max_walk_time = cfg_yaml$routing$max_walk_time,
      walk_speed = cfg_yaml$routing$walk_speed,
      max_trip_duration = cfg_yaml$routing$max_trip_duration,
      max_rides = cfg_yaml$routing$max_rides,
      max_lts = cfg_yaml$routing$max_lts %||% 2,
      percentiles = cfg_yaml$routing$percentiles %||% list(50),
      service_area_buffer_m = cfg_yaml$routing$service_area_buffer_m %||% 2000,
      restrict_to_service_area = isTRUE(cfg_yaml$routing$restrict_to_service_area),
      origin_chunk_size = cfg_yaml$routing$origin_chunk_size %||% 50,
      destination_chunk_size = cfg_yaml$routing$destination_chunk_size %||% 200,
      date_strategy = cfg_yaml$routing$date_strategy %||% "all_dates",
      sample_n_dates_per_period = cfg_yaml$routing$sample_n_dates_per_period %||% 3,
      routing_windows = cfg_yaml$routing$routing_windows,
      unreachable_penalty_minutes = cfg_yaml$routing$unreachable_penalty_minutes %||% cfg_yaml$routing$max_trip_duration,
      n_threads = cfg_yaml$routing$n_threads %||% Inf,
      java_memory = cfg_yaml$r5r_network$java_memory %||% "12G",
      java_active_processors = cfg_yaml$r5r_network$java_active_processors %||% 2,
      overwrite_networks = isTRUE(cfg_yaml$r5r_network$overwrite_networks)
    ),
    map = cfg_yaml$map %||% list(default_comparison_id = "P3_vs_P1", default_time_window_id = "peak"),
    optional_covariates = cfg_yaml$optional_covariates %||% list(download_population = FALSE),
    run_management = cfg_yaml$run_management %||% list(run_label = NULL),
    run_options = make_run_options(cfg_yaml$run_options %||% list())
  )
}

normalize_list_config <- function(cfg_raw, config_path) {
  city_id <- cfg_raw$city_id

  list(
    project = list(
      city_id = city_id,
      city_label = cfg_raw$city_label,
      timezone = project_default_timezone(city_id),
      survey_name = city_id,
      survey_wave = cfg_raw$active_survey_source_id,
      config_path = config_path
    ),
    survey_sources = make_survey_sources_from_list(cfg_raw),
    active_survey_source_id = cfg_raw$active_survey_source_id,
    analysis_area = list(
      counties = make_county_specs_from_list_config(cfg_raw),
      tract_year = cfg_raw$study_area$tract_year,
      county_outline_year = cfg_raw$study_area$county_year,
      zip_zcta_year = as.integer(cfg_raw$analysis_area$zip_zcta_year %||% 2020),
      taz_file = cfg_raw$analysis_area$taz_file %||% NULL,
      taz_id_col = cfg_raw$analysis_area$taz_id_col %||% "taz_id",
      taz_name_col = cfg_raw$analysis_area$taz_name_col %||% NULL
    ),
    geography = list(
      analysis_unit = normalize_analysis_unit(cfg_raw$geography$analysis_unit %||% "tract"),
      service_buffer_m = cfg_raw$geography$service_buffer_m,
      restrict_to_gtfs_service_area = isTRUE(cfg_raw$geography$restrict_to_gtfs_service_area)
    ),
    study_periods = normalize_periods_tbl(cfg_raw$periods),
    analysis_calendar = list(
      weekday_only = isTRUE(cfg_raw$weekday_only),
      excluded_dates = as.Date(cfg_raw$excluded_dates)
    ),
    od_scenarios = make_od_scenarios_from_list(cfg_raw),
    od_settings = list(
      minimum_weight = cfg_raw$od_weights$minimum_weight %||% 0,
      minimum_weight_share_within_origin = cfg_raw$od_weights$minimum_weight_share_within_origin %||% 0,
      max_destinations_per_origin = cfg_raw$od_weights$max_destinations_per_origin %||% Inf
    ),
    synthetic_survey = list(
      enabled = isTRUE(cfg_raw$synthetic_survey$enabled),
      max_travel_minutes = cfg_raw$synthetic_survey$max_travel_minutes %||% 180,
      include_same_origin_destination = isTRUE(cfg_raw$synthetic_survey$include_same_origin_destination)
    ),
    auxiliary_weights = list(
      origin_multiplier_file = NULL,
      origin_id_col = "GEOID",
      origin_multiplier_col = "multiplier",
      destination_multiplier_file = if (isTRUE(cfg_raw$od_weights$use_destination_population_multiplier)) cfg_raw$optional_covariates$tract_covariates_file else NULL,
      destination_id_col = "GEOID",
      destination_multiplier_col = cfg_raw$od_weights$destination_population_field
    ),
    gtfs_feeds = make_gtfs_feeds_from_list(cfg_raw),
    osm = list(
      download_url = cfg_raw$osm$pbf_url,
      local_pbf_path = cfg_raw$osm$pbf_file
    ),
    routing = list(
      modes = as.list(cfg_raw$routing$mode),
      max_walk_time = cfg_raw$routing$max_walk_time,
      walk_speed = cfg_raw$routing$walk_speed_kmh,
      max_trip_duration = cfg_raw$routing$max_trip_duration,
      max_rides = cfg_raw$routing$max_rides,
      max_lts = cfg_raw$routing$max_lts,
      percentiles = list(50),
      service_area_buffer_m = cfg_raw$geography$service_buffer_m,
      restrict_to_service_area = isTRUE(cfg_raw$geography$restrict_to_gtfs_service_area),
      origin_chunk_size = cfg_raw$routing$origin_batch_size,
      destination_chunk_size = cfg_raw$routing$destination_chunk_size %||% 200,
      date_strategy = "sample_n_dates_per_period",
      sample_n_dates_per_period = 3,
      routing_windows = make_routing_windows_from_list(cfg_raw),
      unreachable_penalty_minutes = cfg_raw$routing$max_trip_duration,
      n_threads = cfg_raw$routing$n_threads,
      java_memory = cfg_raw$routing$java_memory %||% "12G",
      java_active_processors = cfg_raw$routing$java_active_processors %||% 2,
      overwrite_networks = FALSE
    ),
    map = list(
      default_comparison_id = "P3_vs_P1",
      default_time_window_id = names(cfg_raw$time_windows)[[1]],
      comparison_metrics = cfg_raw$map$comparison_metrics,
      period_metrics = cfg_raw$map$period_metrics
    ),
    optional_covariates = cfg_raw$optional_covariates,
    run_management = list(run_label = cfg_raw$run_label %||% NULL),
    run_options = make_run_options(cfg_raw$run_options %||% list())
  )
}

resolve_active_survey_source <- function(cfg, source_id = NULL) {
  survey_sources <- cfg$survey_sources
  source_id_use <- source_id %||% cfg$active_survey_source_id
  ids <- purrr::map_chr(survey_sources, "source_id")

  if (!source_id_use %in% ids) {
    stop(paste0("Unknown survey source id: ", source_id_use), call. = FALSE)
  }

  survey_sources[[match(source_id_use, ids)]]
}

project_root_from_config <- function(config_path) {
  normalizePath(file.path(dirname(config_path), ".."), winslash = "/", mustWork = FALSE)
}

extract_analysis_config_for_signature <- function(cfg) {
  list(
    project = list(
      city_id = cfg$project$city_id,
      timezone = cfg$project$timezone,
      active_survey_source_id = cfg$active_survey_source_id
    ),
    analysis_area = cfg$analysis_area,
    geography = cfg$geography,
    study_periods = cfg$study_periods,
    analysis_calendar = cfg$analysis_calendar,
    od_scenarios = cfg$od_scenarios,
    od_settings = cfg$od_settings,
    synthetic_survey = cfg$synthetic_survey,
    auxiliary_weights = cfg$auxiliary_weights,
    gtfs_feeds = cfg$gtfs_feeds,
    osm = cfg$osm,
    routing = list(
      modes = cfg$routing$modes,
      max_walk_time = cfg$routing$max_walk_time,
      walk_speed = cfg$routing$walk_speed,
      max_trip_duration = cfg$routing$max_trip_duration,
      max_rides = cfg$routing$max_rides,
      max_lts = cfg$routing$max_lts,
      percentiles = cfg$routing$percentiles,
      service_area_buffer_m = cfg$routing$service_area_buffer_m,
      restrict_to_service_area = cfg$routing$restrict_to_service_area,
      origin_chunk_size = cfg$routing$origin_chunk_size,
      destination_chunk_size = cfg$routing$destination_chunk_size,
      date_strategy = cfg$routing$date_strategy,
      sample_n_dates_per_period = cfg$routing$sample_n_dates_per_period,
      routing_windows = cfg$routing$routing_windows,
      unreachable_penalty_minutes = cfg$routing$unreachable_penalty_minutes
    ),
    map = cfg$map
  )
}

extract_runtime_config <- function(cfg) {
  list(
    java_memory = cfg$routing$java_memory,
    java_active_processors = cfg$routing$java_active_processors,
    n_threads = cfg$routing$n_threads,
    overwrite_networks = cfg$routing$overwrite_networks,
    run_options = cfg$run_options
  )
}

compute_analysis_signature <- function(cfg) {
  analysis_cfg <- extract_analysis_config_for_signature(cfg)
  json_txt <- jsonlite::toJSON(analysis_cfg, auto_unbox = TRUE, null = "null", pretty = FALSE)
  list(
    analysis_config = analysis_cfg,
    runtime_config = extract_runtime_config(cfg),
    analysis_signature = text_md5(json_txt)
  )
}

build_project_paths <- function(cfg, source_id, run_id) {
  project_root <- project_root_from_config(cfg$project$config_path)
  city_id <- cfg$project$city_id
  unit_id <- normalize_analysis_unit(cfg$geography$analysis_unit %||% "tract")
  run_root <- file.path(project_root, "data", "processed", city_id, "runs", source_id, unit_id, run_id)
  list(
    project_root = project_root,
    run_id = run_id,
    run_root = run_root,
    raw_survey_dir = file.path(project_root, "data", "raw", "surveys", city_id),
    raw_gtfs_dir = file.path(project_root, "data", "raw", "gtfs", city_id),
    raw_osm_dir = file.path(project_root, "data", "raw", "osm", city_id),
    survey_dir = file.path(project_root, "data", "processed", city_id, "survey", source_id),
    geography_dir = file.path(project_root, "data", "processed", city_id, "geography", cfg$geography$analysis_unit %||% "tract"),
    service_area_dir = file.path(run_root, "geography"),
    od_dir = file.path(run_root, "od"),
    gtfs_output_dir = file.path(run_root, "gtfs_supply"),
    network_dir = file.path(project_root, "data", "processed", city_id, "networks"),
    travel_time_dir = file.path(run_root, "travel_times"),
    accessibility_dir = file.path(run_root, "accessibility"),
    metadata_dir = file.path(run_root, "metadata"),
    maps_dir = file.path(project_root, "outputs", city_id, "maps", source_id, unit_id, run_id),
    logs_dir = file.path(project_root, "logs", city_id, source_id, unit_id, run_id)
  )
}

ensure_project_dirs <- function(cfg) {
  dir_paths <- cfg$paths
  dir_paths$raw_osm_dir <- NULL
  dir_paths <- dir_paths[stringr::str_detect(names(dir_paths), "(_dir|_root)$")]
  purrr::walk(unique(unlist(dir_paths, use.names = FALSE)), fs::dir_create)
  invisible(cfg)
}

load_project_config <- function(config_path, source_id = NULL) {
  ext <- tolower(tools::file_ext(config_path))

  if (ext %in% c("yml", "yaml")) {
    cfg <- normalize_yaml_config(yaml::read_yaml(config_path), config_path)
  } else if (ext == "r") {
    env <- new.env(parent = baseenv())
    sys.source(config_path, envir = env)
    if (!exists("project_config", envir = env, inherits = FALSE)) {
      stop("R config must define object `project_config`.", call. = FALSE)
    }
    cfg <- normalize_list_config(get("project_config", envir = env, inherits = FALSE), config_path)
  } else {
    stop("Unsupported config format. Use .R, .yml, or .yaml", call. = FALSE)
  }

  active_source <- resolve_active_survey_source(cfg, source_id)
  cfg$active_survey_source <- active_source
  cfg$active_survey_source_id <- active_source$source_id

  sig <- compute_analysis_signature(cfg)
  sig_short <- substr(sig$analysis_signature, 1, 12)
  run_label <- sanitize_path_component(cfg$run_management$run_label %||% "")
  unit_label <- sanitize_path_component(cfg$geography$analysis_unit %||% "tract")
  run_id <- if (nzchar(run_label) && run_label != "item") paste0(run_label, "__", unit_label, "__", sig_short) else paste0(unit_label, "__", sig_short)

  cfg$run <- list(
    run_label = if (run_label == "item") "" else run_label,
    run_id = run_id,
    analysis_signature = sig$analysis_signature,
    analysis_signature_short = sig_short,
    analysis_config = sig$analysis_config,
    runtime_config = sig$runtime_config
  )

  cfg$paths <- build_project_paths(cfg, active_source$source_id, run_id)
  cfg <- validate_normalized_config(cfg)
  cfg
}

parse_args_config <- function(default_config = NULL, project_root = getwd()) {
  args <- commandArgs(trailingOnly = TRUE)

  config_path <- if (length(args) == 0) {
    if (is.null(default_config)) {
      stop("Please provide a config path.", call. = FALSE)
    }
    default_config
  } else {
    args[[1]]
  }

  if (!fs::is_absolute_path(config_path)) {
    config_path <- file.path(project_root, config_path)
  }
  config_path <- normalizePath(config_path, winslash = "/", mustWork = FALSE)

  list(
    config_path = config_path,
    source_id = if (length(args) >= 2) args[[2]] else NULL
  )
}

apply_runtime_overrides <- function(cfg, overrides = list()) {
  if (length(overrides) == 0) return(cfg)

  if (!is.null(overrides$analysis_unit)) {
    cfg$geography$analysis_unit <- normalize_analysis_unit(overrides$analysis_unit)
  }
  if (!is.null(overrides$run_label)) {
    cfg$run_management$run_label <- as.character(overrides$run_label)
  }
  if (!is.null(overrides$force_all)) {
    cfg$run_options$force <- isTRUE(overrides$force_all)
  }
  if (!is.null(overrides$synthetic_survey_enabled)) {
    cfg$synthetic_survey$enabled <- isTRUE(overrides$synthetic_survey_enabled)
  }
  if (!is.null(overrides$synthetic_survey_max_travel_minutes)) {
    cfg$synthetic_survey$max_travel_minutes <- safe_numeric(overrides$synthetic_survey_max_travel_minutes)
  }

  sig <- compute_analysis_signature(cfg)
  sig_short <- substr(sig$analysis_signature, 1, 12)
  run_label <- sanitize_path_component(cfg$run_management$run_label %||% "")
  unit_label <- sanitize_path_component(cfg$geography$analysis_unit %||% "tract")
  run_id <- if (nzchar(run_label) && run_label != "item") paste0(run_label, "__", unit_label, "__", sig_short) else paste0(unit_label, "__", sig_short)

  cfg$run <- list(
    run_label = if (run_label == "item") "" else run_label,
    run_id = run_id,
    analysis_signature = sig$analysis_signature,
    analysis_signature_short = sig_short,
    analysis_config = sig$analysis_config,
    runtime_config = sig$runtime_config
  )
  cfg$paths <- build_project_paths(cfg, cfg$active_survey_source_id, run_id)
  cfg
}
