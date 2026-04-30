load_project <- function(project_root = getwd()) {
  source(file.path(project_root, "R", "packages.R"), local = .GlobalEnv)
  load_required_packages()

  source(file.path(project_root, "R", "utils_common.R"), local = .GlobalEnv)
  source(file.path(project_root, "R", "utils_config.R"), local = .GlobalEnv)
  source(file.path(project_root, "R", "utils_survey_common.R"), local = .GlobalEnv)
  source(file.path(project_root, "R", "adapters_cmap_phase1.R"), local = .GlobalEnv)
  source(file.path(project_root, "R", "adapters_cmap_2018_2019.R"), local = .GlobalEnv)
  source(file.path(project_root, "R", "adapters_dvrpc_public.R"), local = .GlobalEnv)
  source(file.path(project_root, "R", "adapters_massachusetts_2011.R"), local = .GlobalEnv)
  source(file.path(project_root, "R", "od_weights.R"), local = .GlobalEnv)
  source(file.path(project_root, "R", "geography.R"), local = .GlobalEnv)
  source(file.path(project_root, "R", "gtfs_summary.R"), local = .GlobalEnv)
  source(file.path(project_root, "R", "routing_r5r.R"), local = .GlobalEnv)
  source(file.path(project_root, "R", "accessibility_metrics.R"), local = .GlobalEnv)
  source(file.path(project_root, "R", "maps_leaflet.R"), local = .GlobalEnv)
  source(file.path(project_root, "R", "utils_cleanup.R"), local = .GlobalEnv)
}


reload_cfg_for_source <- function(cfg, source_id = NULL) {
  source_id_use <- source_id %||% cfg$active_survey_source_id
  if (identical(source_id_use, cfg$active_survey_source_id)) {
    return(cfg)
  }

  cfg2 <- load_project_config(cfg$project$config_path, source_id_use)
  apply_runtime_overrides(cfg2, list(
    analysis_unit = cfg$geography$analysis_unit,
    run_label = cfg$run_management$run_label,
    force_all = cfg$run_options$force,
    synthetic_survey_enabled = cfg$synthetic_survey$enabled,
    synthetic_survey_max_travel_minutes = cfg$synthetic_survey$max_travel_minutes
  ))
}

survey_outputs_exist <- function(cfg) {
  all_files_nonempty(c(
    file.path(cfg$paths$survey_dir, "households_std.csv.gz"),
    file.path(cfg$paths$survey_dir, "persons_std.csv.gz"),
    file.path(cfg$paths$survey_dir, "trips_std.csv.gz"),
    file.path(cfg$paths$survey_dir, "survey_metadata.json")
  ), min_bytes = 50)
}

od_outputs_exist <- function(cfg) {
  all_files_nonempty(c(
    file.path(cfg$paths$od_dir, "od_weights_all.csv.gz"),
    file.path(cfg$paths$od_dir, "od_marginals_origin.csv"),
    file.path(cfg$paths$od_dir, "od_marginals_destination.csv"),
    file.path(cfg$paths$od_dir, "top_od_pairs.csv")
  ), min_bytes = 50)
}

gtfs_outputs_exist <- function(cfg) {
  all_files_nonempty(c(
    file.path(cfg$paths$gtfs_output_dir, "period_summary.csv"),
    file.path(cfg$paths$gtfs_output_dir, "headway_compare.csv"),
    file.path(cfg$paths$gtfs_output_dir, "trip_compare.csv")
  ), min_bytes = 50)
}

analysis_geography_outputs_exist <- function(cfg) {
  required_files <- c(
    file.path(cfg$paths$geography_dir, "analysis_zones.gpkg"),
    file.path(cfg$paths$geography_dir, "analysis_zone_centroids.gpkg"),
    file.path(cfg$paths$geography_dir, "tract_to_analysis_zone.csv")
  )
  if (isTRUE(cfg$geography$restrict_to_gtfs_service_area)) {
    required_files <- c(required_files, cfg$paths$service_area_path, cfg$paths$zones_served_path, cfg$paths$zone_centroids_served_path)
  }
  all_files_nonempty(required_files, min_bytes = 50)
}

accessibility_outputs_exist <- function(cfg) {
  all_files_nonempty(c(
    file.path(cfg$paths$accessibility_dir, "zone_period_metrics.csv"),
    file.path(cfg$paths$accessibility_dir, "zone_period_comparisons.csv"),
    file.path(cfg$paths$accessibility_dir, "zone_period_metrics.gpkg"),
    file.path(cfg$paths$accessibility_dir, "zone_period_comparisons.gpkg")
  ), min_bytes = 50)
}

maps_outputs_exist <- function(cfg) {
  file_is_nonempty(file.path(cfg$paths$maps_dir, "index.html"), min_bytes = 50)
}

run_download_geography <- function(cfg) {
  ensure_project_dirs(cfg)

  tract_path <- file.path(cfg$paths$geography_dir, "tracts.gpkg")
  centroid_path <- file.path(cfg$paths$geography_dir, "tract_centroids.gpkg")
  county_path <- file.path(cfg$paths$geography_dir, "county_outlines.gpkg")

  have_base_geo <- all_files_nonempty(c(tract_path, centroid_path, county_path), 100)
  have_osm <- file_is_nonempty(cfg$osm$local_pbf_path, 1024 * 1024)
  have_analysis_geo <- analysis_geography_outputs_exist(cfg)

  if (!isTRUE(cfg$run_options$force) && !isTRUE(cfg$run_options$force_downloads) && have_base_geo && have_osm && have_analysis_geo) {
    message("Geography and OSM already prepared for run ", cfg$run$run_id, ". Skipping download step.")
    return(get_active_geography_for_routing(cfg))
  }

  message("Downloading tract and county geography for ", cfg$project$city_label, "...")
  download_osm_pbf_if_needed(cfg)
  geography_outputs <- download_county_tract_geography(cfg)
  build_service_area(cfg, geography_outputs)
  out <- get_active_geography_for_routing(cfg)
  write_run_metadata(cfg)
  invisible(out)
}

run_download_optional_covariates <- function(cfg) {
  ensure_project_dirs(cfg)
  maybe_download_optional_covariates(cfg)
}


run_standardize_surveys <- function(cfg, source_ids = NULL) {
  source_id_use <- source_ids %||% cfg$active_survey_source_id
  cfg2 <- reload_cfg_for_source(cfg, source_id_use)
  ensure_project_dirs(cfg2)

  if (!isTRUE(cfg2$run_options$force) && !isTRUE(cfg2$run_options$force_standardize) && survey_outputs_exist(cfg2)) {
    message("Standardized survey outputs already exist for ", cfg2$active_survey_source_id, ". Skipping.")
    return(read_standardized_survey(cfg2))
  }

  message("Standardizing survey source: ", cfg2$active_survey_source_id)
  adapter <- get_survey_adapter(cfg2$active_survey_source$adapter)
  std_list <- adapter(cfg2)
  save_standardized_survey(std_list, cfg2$paths$survey_dir)
  write_run_metadata(cfg2)
  invisible(std_list)
}

run_build_od_weights <- function(cfg, source_id = NULL) {
  cfg2 <- reload_cfg_for_source(cfg, source_id %||% cfg$active_survey_source_id)
  ensure_project_dirs(cfg2)

  if (!isTRUE(cfg2$run_options$force) && !isTRUE(cfg2$run_options$force_od) && od_outputs_exist(cfg2)) {
    message("OD weights already exist for run ", cfg2$run$run_id, ". Skipping.")
    return(read_od_weights(cfg2))
  }

  out <- build_all_od_weights(cfg2)
  write_run_metadata(cfg2)
  out
}

run_gtfs_supply_compare <- function(cfg) {
  ensure_project_dirs(cfg)

  if (!isTRUE(cfg$run_options$force) && !isTRUE(cfg$run_options$force_gtfs) && gtfs_outputs_exist(cfg)) {
    message("GTFS supply summaries already exist for run ", cfg$run$run_id, ". Skipping.")
    return(invisible(NULL))
  }

  out <- summarize_gtfs_by_period(cfg)
  write_run_metadata(cfg)
  out
}

run_compute_od_travel_times <- function(cfg) {
  ensure_project_dirs(cfg)
  out <- compute_all_travel_times(cfg)
  write_run_metadata(cfg)
  out
}

run_aggregate_metrics <- function(cfg) {
  ensure_project_dirs(cfg)

  out_metrics <- file.path(cfg$paths$accessibility_dir, "zone_period_metrics.csv")
  out_comp <- file.path(cfg$paths$accessibility_dir, "zone_period_comparisons.csv")
  input_paths <- c(file.path(cfg$paths$travel_time_dir, "period_travel_times.csv.gz"), file.path(cfg$paths$od_dir, "od_weights_all.csv.gz"))

  if (!isTRUE(cfg$run_options$force) && !isTRUE(cfg$run_options$force_metrics) && accessibility_outputs_exist(cfg) && output_is_fresh(out_metrics, input_paths, 50) && output_is_fresh(out_comp, input_paths, 50)) {
    message("Accessibility metrics already exist and are up to date for run ", cfg$run$run_id, ". Skipping.")
    return(invisible(NULL))
  }

  out <- compute_period_metrics(cfg)
  write_run_metadata(cfg)
  out
}

run_make_maps <- function(cfg) {
  ensure_project_dirs(cfg)

  index_path <- file.path(cfg$paths$maps_dir, "index.html")
  input_paths <- c(
    file.path(cfg$paths$accessibility_dir, "zone_period_metrics.gpkg"),
    file.path(cfg$paths$accessibility_dir, "zone_period_comparisons.gpkg"),
    file.path(cfg$paths$od_dir, "od_marginals_origin.csv"),
    file.path(cfg$paths$od_dir, "od_marginals_destination.csv"),
    file.path(cfg$paths$od_dir, "top_od_pairs.csv")
  )

  if (!isTRUE(cfg$run_options$force) && !isTRUE(cfg$run_options$force_maps) && maps_outputs_exist(cfg) && output_is_fresh(index_path, input_paths, 50)) {
    message("Interactive maps already exist and are up to date for run ", cfg$run$run_id, ". Skipping.")
    return(invisible(NULL))
  }

  out <- make_all_interactive_maps(cfg)
  write_run_metadata(cfg)
  out
}
