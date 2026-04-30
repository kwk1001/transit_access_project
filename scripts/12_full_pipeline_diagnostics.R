get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep('^--file=', cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub('^--file=', '', file_arg[[1]]), winslash = '/', mustWork = FALSE))
  }
  NULL
}

bootstrap_config_args <- function(default_config, project_root) {
  args <- commandArgs(trailingOnly = TRUE)
  config_path <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else default_config
  if (!grepl('^(~|/|[A-Za-z]:)', config_path)) {
    config_path <- file.path(project_root, config_path)
  }
  source_id <- if (length(args) >= 2 && nzchar(args[[2]])) args[[2]] else NULL
  analysis_unit <- if (length(args) >= 3 && nzchar(args[[3]])) tolower(args[[3]]) else NULL
  list(
    config_path = normalizePath(config_path, winslash = '/', mustWork = FALSE),
    source_id = source_id,
    analysis_unit = analysis_unit
  )
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

safe_json_read <- function(path) {
  tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) NULL)
}

count_rows_quick <- function(path) {
  out <- tryCatch(readr::count_fields(path, tokenizer = readr::tokenizer_csv()), error = function(e) integer())
  if (length(out) <= 1) return(0L)
  as.integer(length(out) - 1L)
}

parse_gtfs_structural_diag <- function(gtfs_zip_path, feed_name) {
  td <- tempfile('gtfs_struct_')
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(gtfs_zip_path, exdir = td)

  read_gtfs_txt <- function(name, cols = NULL) {
    p <- file.path(td, name)
    if (!file.exists(p)) return(NULL)
    readr::read_csv(p, col_types = cols %||% readr::cols(.default = readr::col_character()), show_col_types = FALSE, progress = FALSE)
  }

  trips <- read_gtfs_txt('trips.txt')
  stops <- read_gtfs_txt('stops.txt')
  stop_times <- read_gtfs_txt('stop_times.txt')
  calendar <- read_gtfs_txt('calendar.txt')
  calendar_dates <- read_gtfs_txt('calendar_dates.txt')

  if (is.null(trips) || is.null(stops) || is.null(stop_times)) {
    return(tibble::tibble(
      feed_name = feed_name,
      gtfs_zip_path = gtfs_zip_path,
      error = 'missing one or more required GTFS tables'
    ))
  }

  stop_times_seq <- stop_times %>%
    dplyr::mutate(
      stop_sequence_num = suppressWarnings(as.integer(stop_sequence)),
      arrival_sec = hms_to_seconds(arrival_time),
      departure_sec = hms_to_seconds(departure_time)
    )

  seq_diag <- stop_times_seq %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(
      n_duplicate_stop_sequence = sum(duplicated(stop_sequence_num) & !is.na(stop_sequence_num)),
      n_non_monotonic_stop_sequence = sum(diff(stop_sequence_num) <= 0, na.rm = TRUE),
      n_non_monotonic_time = sum(diff(dplyr::coalesce(departure_sec, arrival_sec)) < 0, na.rm = TRUE),
      .groups = 'drop'
    )

  missing_trip_refs <- sum(!stop_times_seq$trip_id %in% trips$trip_id, na.rm = TRUE)
  missing_stop_refs <- sum(!stop_times_seq$stop_id %in% stops$stop_id, na.rm = TRUE)
  trips_without_stop_times <- sum(!trips$trip_id %in% stop_times_seq$trip_id, na.rm = TRUE)

  tibble::tibble(
    feed_name = feed_name,
    gtfs_zip_path = gtfs_zip_path,
    n_routes = if (!is.null(read_gtfs_txt('routes.txt'))) nrow(read_gtfs_txt('routes.txt')) else NA_integer_,
    n_trips = nrow(trips),
    n_stop_times = nrow(stop_times_seq),
    n_stops = nrow(stops),
    n_calendar = if (!is.null(calendar)) nrow(calendar) else 0L,
    n_calendar_dates = if (!is.null(calendar_dates)) nrow(calendar_dates) else 0L,
    n_duplicate_stop_sequence = sum(seq_diag$n_duplicate_stop_sequence, na.rm = TRUE),
    n_non_monotonic_stop_sequence = sum(seq_diag$n_non_monotonic_stop_sequence, na.rm = TRUE),
    n_non_monotonic_time = sum(seq_diag$n_non_monotonic_time, na.rm = TRUE),
    n_missing_trip_refs = missing_trip_refs,
    n_missing_stop_refs = missing_stop_refs,
    n_trips_without_stop_times = trips_without_stop_times
  )
}

routing_point_diagnostics <- function(zone_centroids_sf, zone_ids, cfg) {
  zone_ids_std <- standardize_zone_id(zone_ids, cfg$geography$analysis_unit)
  zone_ids_std <- unique(zone_ids_std[!is.na(zone_ids_std) & nzchar(zone_ids_std)])

  centroids_tbl <- zone_centroids_sf %>%
    dplyr::mutate(zone_id = standardize_zone_id(zone_id, cfg$geography$analysis_unit)) %>%
    dplyr::filter(!is.na(zone_id), nzchar(zone_id)) %>%
    sf::st_transform(4326) %>%
    dplyr::mutate(
      lon = sf::st_coordinates(.)[, 1],
      lat = sf::st_coordinates(.)[, 2]
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(
      zone_id = as.character(zone_id),
      lon = as.numeric(lon),
      lat = as.numeric(lat),
      coord_valid = !is.na(lon) & !is.na(lat) & is.finite(lon) & is.finite(lat) & lon >= -180 & lon <= 180 & lat >= -90 & lat <= 90
    )

  found_tbl <- tibble::tibble(zone_id = zone_ids_std) %>%
    dplyr::left_join(centroids_tbl, by = 'zone_id') %>%
    dplyr::mutate(
      found_in_centroids = !is.na(lon) | !is.na(lat) | zone_id %in% centroids_tbl$zone_id,
      issue = dplyr::case_when(
        !found_in_centroids ~ 'missing_in_routing_centroids',
        !coord_valid ~ 'invalid_coordinates_after_extraction',
        TRUE ~ 'ok'
      )
    )

  list(
    summary = found_tbl %>% dplyr::count(issue, name = 'n') %>% dplyr::arrange(issue),
    details = found_tbl %>% dplyr::arrange(issue, zone_id),
    valid_points = found_tbl %>% dplyr::filter(issue == 'ok')
  )
}

extract_failure_logs <- function(log_dir) {
  files <- if (dir.exists(log_dir)) fs::dir_ls(log_dir, glob = 'routing_origin_fallback_*.json') else character()
  if (length(files) == 0) {
    return(list(summary = tibble::tibble(), failed_origins = tibble::tibble(), files = character()))
  }

  summary_rows <- list()
  failed_rows <- list()
  for (f in files) {
    j <- safe_json_read(f)
    if (is.null(j)) next
    ctx <- j$context %||% list()
    summary_rows[[length(summary_rows) + 1L]] <- tibble::tibble(
      log_file = basename(f),
      log_path = f,
      failure_type = as.character(j$failure_type %||% NA_character_),
      feed_name = as.character(ctx$feed_name %||% NA_character_),
      analysis_date = as.character(ctx$analysis_date %||% NA_character_),
      period_id = as.character(ctx$period_id %||% NA_character_),
      time_window_id = as.character(ctx$time_window_id %||% NA_character_),
      od_scenario_id = as.character(ctx$od_scenario_id %||% NA_character_),
      origin_chunk_id = suppressWarnings(as.integer(ctx$origin_chunk_id %||% NA_integer_)),
      destination_chunk_id = suppressWarnings(as.integer(ctx$destination_chunk_id %||% NA_integer_)),
      od_pairs_in_chunk = suppressWarnings(as.integer(ctx$od_pairs_in_chunk %||% NA_integer_)),
      n_failed_origins = if (is.list(j$failed_origins)) length(j$failed_origins) else 0L
    )

    fo <- j$failed_origins
    if (is.null(fo)) next
    if (is.data.frame(fo)) {
      fo_df <- tibble::as_tibble(fo)
    } else if (is.list(fo)) {
      fo_df <- purrr::map_dfr(fo, function(x) {
        tibble::tibble(
          origin_id = as.character(x$origin_id %||% NA_character_),
          error = as.character(x$error %||% NA_character_)
        )
      })
    } else {
      fo_df <- tibble::tibble()
    }
    if (nrow(fo_df) == 0) next
    fo_df <- fo_df %>%
      dplyr::mutate(
        log_file = basename(f),
        feed_name = as.character(ctx$feed_name %||% NA_character_),
        analysis_date = as.character(ctx$analysis_date %||% NA_character_),
        period_id = as.character(ctx$period_id %||% NA_character_),
        time_window_id = as.character(ctx$time_window_id %||% NA_character_),
        od_scenario_id = as.character(ctx$od_scenario_id %||% NA_character_),
        origin_chunk_id = suppressWarnings(as.integer(ctx$origin_chunk_id %||% NA_integer_)),
        destination_chunk_id = suppressWarnings(as.integer(ctx$destination_chunk_id %||% NA_integer_))
      )
    failed_rows[[length(failed_rows) + 1L]] <- fo_df
  }

  list(
    summary = dplyr::bind_rows(summary_rows),
    failed_origins = dplyr::bind_rows(failed_rows),
    files = files
  )
}

summarize_daily_outputs <- function(cfg) {
  daily_dir <- file.path(cfg$paths$travel_time_dir, 'daily')
  files <- if (dir.exists(daily_dir)) fs::dir_ls(daily_dir, glob = '*.csv.gz') else character()
  if (length(files) == 0) return(tibble::tibble())
  tibble::tibble(
    file = basename(files),
    path = files,
    size_bytes = file.info(files)$size,
    n_rows = purrr::map_int(files, count_rows_quick)
  ) %>%
    dplyr::mutate(is_header_only_or_empty = n_rows == 0L)
}

reconstruct_failure_context_points <- function(cfg, geography_outputs, failure_summary, od_all) {
  if (nrow(failure_summary) == 0) {
    return(list(origins = tibble::tibble(), destinations = tibble::tibble()))
  }

  routing_dates <- choose_routing_dates(cfg)
  windows_tbl <- tibble::as_tibble(dplyr::bind_rows(cfg$routing$routing_windows)) %>%
    dplyr::mutate(time_window_id = as.character(time_window_id), od_scenario_id = as.character(od_scenario_id))

  routing_centroids <- geography_outputs$routing_zone_centroids %||% geography_outputs$analysis_zone_centroids
  origin_out <- list()
  dest_out <- list()

  failure_summary2 <- failure_summary %>%
    dplyr::mutate(
      analysis_date = as.Date(analysis_date),
      origin_chunk_id = as.integer(origin_chunk_id),
      destination_chunk_id = as.integer(destination_chunk_id)
    ) %>%
    dplyr::distinct(feed_name, analysis_date, time_window_id, od_scenario_id, origin_chunk_id, destination_chunk_id)

  for (i in seq_len(nrow(failure_summary2))) {
    row_i <- failure_summary2[i, ]
    win <- windows_tbl %>% dplyr::filter(time_window_id == row_i$time_window_id, od_scenario_id == row_i$od_scenario_id) %>% dplyr::slice(1)
    if (nrow(win) == 0) next
    date_row <- routing_dates %>% dplyr::filter(feed_name == row_i$feed_name, analysis_date == row_i$analysis_date) %>% dplyr::slice(1)
    if (nrow(date_row) == 0) next

    od_use <- od_all %>%
      dplyr::filter(scenario_id == row_i$od_scenario_id) %>%
      dplyr::mutate(
        origin_id = as.character(standardize_zone_id(origin_id, cfg$geography$analysis_unit)),
        destination_id = as.character(standardize_zone_id(destination_id, cfg$geography$analysis_unit))
      )

    origins_all <- make_routing_points(routing_centroids, unique(od_use$origin_id), cfg)
    destinations_all <- make_routing_points(routing_centroids, unique(od_use$destination_id), cfg)
    if (nrow(origins_all) == 0 || nrow(destinations_all) == 0) next

    origin_chunks <- split(origins_all, ceiling(seq_len(nrow(origins_all)) / cfg$routing$origin_chunk_size))
    if (row_i$origin_chunk_id < 1 || row_i$origin_chunk_id > length(origin_chunks)) next
    origins_chunk <- origin_chunks[[row_i$origin_chunk_id]]
    od_pairs_chunk <- od_use %>%
      dplyr::filter(origin_id %in% origins_chunk$id) %>%
      dplyr::distinct(origin_id, destination_id)
    if (nrow(od_pairs_chunk) == 0) next

    destinations_needed <- destinations_all %>%
      dplyr::filter(id %in% unique(od_pairs_chunk$destination_id))
    destination_chunk_size <- cfg$routing$destination_chunk_size %||% 200
    destination_chunks <- split(destinations_needed, ceiling(seq_len(nrow(destinations_needed)) / destination_chunk_size))
    if (row_i$destination_chunk_id < 1 || row_i$destination_chunk_id > length(destination_chunks)) next
    dest_chunk <- destination_chunks[[row_i$destination_chunk_id]]

    origin_out[[length(origin_out) + 1L]] <- origins_chunk %>%
      dplyr::mutate(
        feed_name = row_i$feed_name,
        analysis_date = as.character(row_i$analysis_date),
        time_window_id = row_i$time_window_id,
        od_scenario_id = row_i$od_scenario_id,
        origin_chunk_id = row_i$origin_chunk_id,
        destination_chunk_id = row_i$destination_chunk_id,
        role = 'origin_chunk'
      )

    dest_out[[length(dest_out) + 1L]] <- dest_chunk %>%
      dplyr::mutate(
        feed_name = row_i$feed_name,
        analysis_date = as.character(row_i$analysis_date),
        time_window_id = row_i$time_window_id,
        od_scenario_id = row_i$od_scenario_id,
        origin_chunk_id = row_i$origin_chunk_id,
        destination_chunk_id = row_i$destination_chunk_id,
        role = 'destination_chunk'
      )
  }

  list(
    origins = dplyr::bind_rows(origin_out),
    destinations = dplyr::bind_rows(dest_out)
  )
}

script_path <- get_script_path()
project_root <- if (!is.null(script_path)) normalizePath(file.path(dirname(script_path), '..'), winslash = '/', mustWork = FALSE) else getwd()
source(file.path(project_root, 'R', 'packages.R'))
boot <- bootstrap_config_args(default_config = file.path('config', 'boston_mts2011.yml'), project_root = project_root)
java_boot <- peek_java_config(boot$config_path)
configure_java_for_r5r(java_memory = java_boot$java_memory, java_active_processors = java_boot$java_active_processors)
source(file.path(project_root, 'R', 'load_project.R'))
load_project(project_root)

cfg <- load_project_config(boot$config_path, boot$source_id)
if (!is.null(boot$analysis_unit)) {
  cfg <- apply_runtime_overrides(cfg, list(analysis_unit = boot$analysis_unit))
}
ensure_project_dirs(cfg)
fs::dir_create(cfg$paths$logs_dir)

cat('=== Full pipeline diagnostics ===\n')
cat('Config:', boot$config_path, '\n')
cat('Run id:', cfg$run$run_id, '\n')
cat('Logs dir:', cfg$paths$logs_dir, '\n')
cat('Travel time dir:', cfg$paths$travel_time_dir, '\n')

cat('\n[1] Routing dates and calendar service levels\n')
routing_dates <- choose_routing_dates(cfg)
print(routing_dates %>% dplyr::arrange(analysis_date) %>% dplyr::select(period_id, analysis_date, feed_name))
feed_registry <- make_feed_registry_for_routing(cfg)
service_by_feed <- purrr::map_dfr(seq_len(nrow(feed_registry)), function(i) {
  gtfs_file <- feed_registry$gtfs_files[[i]][[1]]
  if (!file.exists(gtfs_file)) {
    return(tibble::tibble(feed_name = feed_registry$feed_name[[i]], analysis_date = as.Date(character()), n_services = integer(), service_ratio = numeric()))
  }
  parse_gtfs_service_levels(gtfs_file) %>%
    dplyr::mutate(feed_name = feed_registry$feed_name[[i]]) %>%
    dplyr::group_by(feed_name) %>%
    dplyr::mutate(service_ratio = ifelse(max(n_services, na.rm = TRUE) > 0, n_services / max(n_services, na.rm = TRUE), NA_real_)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(analysis_date = date)
})
service_selected <- routing_dates %>%
  dplyr::left_join(service_by_feed, by = c('feed_name', 'analysis_date')) %>%
  dplyr::arrange(analysis_date, feed_name)
print(service_selected %>% dplyr::select(period_id, analysis_date, feed_name, n_services, service_ratio))
readr::write_csv(service_selected, file.path(cfg$paths$logs_dir, 'diagnostic_selected_dates_service_levels.csv'))

cat('\n[2] GTFS structural diagnostics\n')
gtfs_diag <- purrr::map_dfr(cfg$gtfs_feeds, function(f) {
  gtfs_path <- as.character(f$gtfs_files[[1]])
  if (!file.exists(gtfs_path)) {
    tibble::tibble(feed_name = as.character(f$feed_name), gtfs_zip_path = gtfs_path, error = 'file_missing')
  } else {
    parse_gtfs_structural_diag(gtfs_path, as.character(f$feed_name))
  }
})
print(gtfs_diag)
readr::write_csv(gtfs_diag, file.path(cfg$paths$logs_dir, 'diagnostic_gtfs_structural.csv'))

cat('\n[3] Routing point diagnostics against current OD\n')
geo <- get_active_geography_for_routing(cfg)
od_path <- file.path(cfg$paths$od_dir, 'od_weights_all.csv.gz')
if (file.exists(od_path)) {
  od_all <- read_csv_guess(od_path)
} else {
  od_all <- tibble::tibble(origin_id = character(), destination_id = character(), scenario_id = character())
}
requested_ids <- unique(c(as.character(od_all$origin_id), as.character(od_all$destination_id)))
requested_ids <- requested_ids[!is.na(requested_ids) & nzchar(requested_ids)]
route_diag <- routing_point_diagnostics(geo$routing_zone_centroids %||% geo$analysis_zone_centroids, requested_ids, cfg)
print(route_diag$summary)
readr::write_csv(route_diag$details, file.path(cfg$paths$logs_dir, 'diagnostic_routing_point_details.csv'))

zone_qc_path <- file.path(cfg$paths$logs_dir, 'zone_point_qc.csv')
if (file.exists(zone_qc_path)) {
  zone_qc <- read_csv_guess(zone_qc_path)
  readr::write_csv(zone_qc, file.path(cfg$paths$logs_dir, 'diagnostic_zone_point_qc_snapshot.csv'))
}

cat('\n[4] Daily output diagnostics\n')
daily_summary <- summarize_daily_outputs(cfg)
if (nrow(daily_summary) == 0) {
  cat('No daily travel time files found.\n')
} else {
  print(daily_summary %>% dplyr::summarise(n_files = dplyr::n(), n_header_only_or_empty = sum(is_header_only_or_empty), total_rows = sum(n_rows)))
  print(daily_summary %>% dplyr::arrange(n_rows, file) %>% dplyr::slice_head(n = 20))
  readr::write_csv(daily_summary, file.path(cfg$paths$logs_dir, 'diagnostic_daily_file_summary.csv'))
}

cat('\n[5] Routing fallback log summary\n')
log_info <- extract_failure_logs(cfg$paths$logs_dir)
if (nrow(log_info$summary) == 0) {
  cat('No routing_origin_fallback_*.json files found in logs_dir.\n')
} else {
  failure_by_chunk <- log_info$summary %>%
    dplyr::group_by(feed_name, analysis_date, time_window_id, od_scenario_id, origin_chunk_id, destination_chunk_id) %>%
    dplyr::summarise(
      n_logs = dplyr::n(),
      n_failed_origins = max(n_failed_origins, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    dplyr::arrange(dplyr::desc(n_failed_origins), feed_name, analysis_date, time_window_id, origin_chunk_id, destination_chunk_id)
  print(failure_by_chunk)
  readr::write_csv(log_info$summary, file.path(cfg$paths$logs_dir, 'diagnostic_failure_logs_raw.csv'))
  readr::write_csv(log_info$failed_origins, file.path(cfg$paths$logs_dir, 'diagnostic_failed_origins_raw.csv'))
  readr::write_csv(failure_by_chunk, file.path(cfg$paths$logs_dir, 'diagnostic_failure_summary_by_chunk.csv'))

  dominant_dest <- failure_by_chunk %>%
    dplyr::count(destination_chunk_id, wt = n_logs, name = 'weighted_logs') %>%
    dplyr::arrange(dplyr::desc(weighted_logs))
  print(dominant_dest)
  readr::write_csv(dominant_dest, file.path(cfg$paths$logs_dir, 'diagnostic_failure_summary_by_destination_chunk.csv'))

  reconstructed <- reconstruct_failure_context_points(cfg, geo, failure_by_chunk, od_all)
  if (nrow(reconstructed$origins) > 0) {
    readr::write_csv(reconstructed$origins, file.path(cfg$paths$logs_dir, 'diagnostic_failure_context_origins.csv'))
    cat('Wrote failure context origins:', nrow(reconstructed$origins), 'rows\n')
  }
  if (nrow(reconstructed$destinations) > 0) {
    readr::write_csv(reconstructed$destinations, file.path(cfg$paths$logs_dir, 'diagnostic_failure_context_destinations.csv'))
    cat('Wrote failure context destinations:', nrow(reconstructed$destinations), 'rows\n')
  }
}

cat('\n[6] Optional network-level checks\n')
if (nrow(log_info$summary) > 0) {
  main_feed <- log_info$summary %>% dplyr::count(feed_name, sort = TRUE) %>% dplyr::slice(1) %>% dplyr::pull(feed_name)
} else if (nrow(routing_dates) > 0) {
  main_feed <- routing_dates %>% dplyr::count(feed_name, sort = TRUE) %>% dplyr::slice(1) %>% dplyr::pull(feed_name)
} else {
  main_feed <- NA_character_
}

if (!is.na(main_feed) && nzchar(main_feed)) {
  gtfs_files <- feed_registry %>% dplyr::filter(feed_name == main_feed) %>% dplyr::pull(gtfs_files) %>% .[[1]]
  network <- NULL
  on.exit({
    if (!is.null(network) && inherits(network, 'r5r_network')) {
      try(r5r::stop_r5(network), silent = TRUE)
    }
  }, add = TRUE)

  network <- tryCatch(build_r5_network_object(cfg, main_feed, gtfs_files), error = function(e) NULL)
  if (inherits(network, 'r5r_network')) {
    if ('check_transit_availability' %in% getNamespaceExports('r5r')) {
      dates_probe <- routing_dates %>% dplyr::filter(feed_name == main_feed) %>% dplyr::distinct(analysis_date) %>% dplyr::arrange(analysis_date)
      availability <- tryCatch(
        r5r::check_transit_availability(network, dates = dates_probe$analysis_date),
        error = function(e) tibble::tibble(error = conditionMessage(e))
      )
      readr::write_csv(tibble::as_tibble(availability), file.path(cfg$paths$logs_dir, 'diagnostic_check_transit_availability.csv'))
      print(availability)
    }

    snap_points <- tibble::tibble()
    failed_ids <- unique(as.character(log_info$failed_origins$origin_id))
    failed_ids <- failed_ids[!is.na(failed_ids) & nzchar(failed_ids)]
    if (length(failed_ids) > 0 && 'find_snap' %in% getNamespaceExports('r5r')) {
      snap_input <- make_routing_points(geo$routing_zone_centroids %||% geo$analysis_zone_centroids, failed_ids, cfg)
      if (nrow(snap_input) > 0) {
        snap_points <- tryCatch(
          tibble::as_tibble(r5r::find_snap(network, points = snap_input, mode = 'WALK', progress = FALSE)),
          error = function(e) tibble::tibble(error = conditionMessage(e))
        )
        readr::write_csv(snap_points, file.path(cfg$paths$logs_dir, 'diagnostic_failed_origin_find_snap.csv'))
        print(head(snap_points, 20))
      }
    }

    if ('r5r_sitrep' %in% getNamespaceExports('r5r')) {
      sitrep <- tryCatch(r5r::r5r_sitrep(), error = function(e) NULL)
      if (!is.null(sitrep)) {
        writeLines(capture.output(print(sitrep)), con = file.path(cfg$paths$logs_dir, 'diagnostic_r5r_sitrep.txt'))
      }
    }
  } else {
    cat('Skipping network-level checks because build_r5_network_object failed.\n')
  }
}

cat('\nDone. Diagnostic artifacts written under:\n')
cat(cfg$paths$logs_dir, '\n')
