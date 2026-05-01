#!/usr/bin/env Rscript

# Diagnose the hypothesis that MBTA commuter-rail canonical trips are triggering
# R5/r5r ArrayIndexOutOfBoundsException.
#
# Usage:
#   Rscript scripts/diagnose_canonical_rail_hypothesis.R <config_path> [feed_name] [analysis_date] [time_window_id] [route_id] [pairs_csv] [gtfs_zip_override]
#
# Examples:
#   Rscript scripts/diagnose_canonical_rail_hypothesis.R config/boston_mts2011.yml MBTA_0825 2025-09-26 midday CR-Fairmount
#   Rscript scripts/diagnose_canonical_rail_hypothesis.R config/boston_mts2011.yml MBTA_0825 2025-09-26 midday "" "" /absolute/path/to/gtfs_variant_rail_route_CR_Fairmount.zip

args <- commandArgs(trailingOnly = TRUE)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
nz <- function(x) !is.null(x) && length(x) > 0 && nzchar(as.character(x[[1]]))
arg_or_null <- function(i) if (length(args) >= i && nz(args[[i]])) args[[i]] else NULL

script_path <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE), error = function(e) NULL)
if (is.null(script_path) || !nzchar(script_path)) {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)
}
project_root <- if (!is.null(script_path) && nzchar(script_path)) normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE) else getwd()

config_path <- arg_or_null(1) %||% file.path(project_root, "config", "boston_mts2011.yml")
if (!grepl("^(~|/|[A-Za-z]:)", config_path)) config_path <- file.path(project_root, config_path)
config_path <- normalizePath(config_path, winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(project_root, "R", "load_project.R"))) {
  inferred_root <- normalizePath(file.path(dirname(config_path), ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(inferred_root, "R", "load_project.R"))) project_root <- inferred_root
}
feed_name_arg <- arg_or_null(2)
analysis_date_arg <- arg_or_null(3)
time_window_id_arg <- arg_or_null(4)
route_id_arg <- arg_or_null(5)
pairs_csv_arg <- arg_or_null(6)
gtfs_zip_override_arg <- arg_or_null(7)
if (!is.null(pairs_csv_arg) && !grepl("^(~|/|[A-Za-z]:)", pairs_csv_arg)) pairs_csv_arg <- file.path(project_root, pairs_csv_arg)
if (!is.null(gtfs_zip_override_arg) && !grepl("^(~|/|[A-Za-z]:)", gtfs_zip_override_arg)) gtfs_zip_override_arg <- file.path(project_root, gtfs_zip_override_arg)

source(file.path(project_root, "R", "packages.R"))
java_boot <- peek_java_config(config_path)
configure_java_for_r5r(java_memory = java_boot$java_memory, java_active_processors = java_boot$java_active_processors, force = TRUE)
source(file.path(project_root, "R", "load_project.R"))
load_project(project_root)

cfg <- load_project_config(config_path)
ensure_project_dirs(cfg)

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")
out_dir <- file.path(cfg$paths$logs_dir, paste0("canonical_rail_probe_", stamp))
fs::dir_create(out_dir)

write_json_pretty <- function(x, path) jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
save_text <- function(txt, path) writeLines(as.character(txt), path, useBytes = TRUE)
safe_run <- function(expr) {
  tryCatch(list(status = "ok", result = force(expr), error_message = NA_character_),
           error = function(e) list(status = "error", result = NULL, error_message = conditionMessage(e)))
}

read_csv_guess <- function(path) readr::read_csv(path, show_col_types = FALSE, progress = FALSE, guess_max = 100000)
read_zip_csv <- function(zip_path, member) {
  listing <- tryCatch(utils::unzip(zip_path, list = TRUE), error = function(e) NULL)
  if (is.null(listing) || !member %in% listing$Name) return(NULL)
  readr::read_csv(unz(zip_path, member), show_col_types = FALSE, progress = FALSE, col_types = readr::cols(.default = "c"))
}
zip_dir_flat <- function(src_dir, zip_path) {
  old <- getwd(); on.exit(setwd(old), add = TRUE)
  setwd(src_dir)
  files <- list.files(src_dir, all.files = FALSE, no.. = TRUE)
  if (length(files) == 0) stop("No files to zip.", call. = FALSE)
  if (file.exists(zip_path)) unlink(zip_path)
  utils::zip(zipfile = zip_path, files = files, flags = "-q")
  zip_path
}

resolve_context <- function(cfg, feed_name_arg, analysis_date_arg, time_window_id_arg) {
  dates <- choose_routing_dates(cfg)
  if (nrow(dates) == 0) stop("No routing dates available from config.", call. = FALSE)
  feed_name <- feed_name_arg %||% dates$feed_name[[1]]
  time_window_id <- time_window_id_arg %||% (cfg$time_windows$time_window_id[[1]] %||% cfg$time_windows$id[[1]] %||% "peak")
  if (is.null(analysis_date_arg)) {
    candidate <- dates %>% dplyr::filter(feed_name == !!feed_name) %>% dplyr::slice(1)
    if (nrow(candidate) == 0) candidate <- dates %>% dplyr::slice(1)
    analysis_date <- as.Date(candidate$analysis_date[[1]])
  } else {
    analysis_date <- as.Date(analysis_date_arg)
  }
  list(feed_name = feed_name, analysis_date = analysis_date, time_window_id = time_window_id)
}

make_feed_registry_single <- function(cfg) {
  reg <- make_feed_registry_for_routing(cfg)
  reg <- reg %>% dplyr::mutate(gtfs_path = purrr::map_chr(gtfs_files, function(x) x[[1]] %||% x))
  reg
}

service_levels_for_context <- function(cfg, feed_name, analysis_date) {
  reg <- make_feed_registry_single(cfg)
  row <- reg %>% dplyr::filter(feed_name == !!feed_name) %>% dplyr::slice(1)
  if (nrow(row) == 0) return(tibble::tibble())
  sl <- safe_run(parse_gtfs_service_levels(row$gtfs_path[[1]]))
  if (!identical(sl$status, "ok") || is.null(sl$result)) return(tibble::tibble())
  out <- sl$result %>% dplyr::mutate(feed_name = feed_name)
  if ("analysis_date" %in% names(out)) {
    out <- out %>% dplyr::mutate(analysis_date = as.Date(.data$analysis_date)) %>% dplyr::filter(.data$analysis_date == as.Date(analysis_date))
  }
  out
}

resolve_gtfs_base <- function(cfg, context, gtfs_zip_override_arg) {
  if (!is.null(gtfs_zip_override_arg) && file.exists(gtfs_zip_override_arg)) {
    return(normalizePath(gtfs_zip_override_arg, winslash = "/", mustWork = TRUE))
  }
  reg <- make_feed_registry_single(cfg)
  row <- reg %>% dplyr::filter(feed_name == !!context$feed_name) %>% dplyr::slice(1)
  if (nrow(row) == 0) stop("Could not resolve GTFS zip for feed ", context$feed_name, call. = FALSE)
  normalizePath(row$gtfs_path[[1]], winslash = "/", mustWork = TRUE)
}

resolve_base_network_osm_pbf <- function(cfg, base_feed_name) {
  candidates <- character()
  if (!is.null(cfg$paths$network_dir) && nzchar(cfg$paths$network_dir)) candidates <- c(candidates, file.path(cfg$paths$network_dir, base_feed_name, "input"))
  if (!is.null(cfg$project$city_id) && nzchar(cfg$project$city_id) && !is.null(cfg$paths$project_root) && nzchar(cfg$paths$project_root)) {
    candidates <- c(candidates, file.path(cfg$paths$project_root, "data", "processed", cfg$project$city_id, "networks", base_feed_name, "input"))
  }
  candidates <- unique(candidates[file.exists(candidates)])
  osm_files <- character()
  for (d in candidates) osm_files <- c(osm_files, fs::dir_ls(d, glob = "*.osm.pbf", recurse = FALSE))
  osm_files <- unique(osm_files[file.exists(osm_files)])
  if (length(osm_files) > 0) return(normalizePath(osm_files[[1]], winslash = "/", mustWork = TRUE))
  cfg_osm <- cfg$osm$local_pbf_path %||% NA_character_
  if (!is.na(cfg_osm) && nzchar(cfg_osm)) {
    if (!grepl("^(~|/|[A-Za-z]:)", cfg_osm)) cfg_osm <- file.path(cfg$paths$project_root, cfg_osm)
    if (file.exists(cfg_osm)) return(normalizePath(cfg_osm, winslash = "/", mustWork = TRUE))
  }
  NA_character_
}

capture_r5r_sitrep <- function(out_path) {
  if (!"r5r_sitrep" %in% getNamespaceExports("r5r")) return(invisible(FALSE))
  txt <- tryCatch(capture.output(r5r::r5r_sitrep()), error = function(e) paste("r5r_sitrep failed:", conditionMessage(e)))
  save_text(txt, out_path)
  invisible(TRUE)
}

build_zone_point_qc <- function(geog, cfg) {
  centroids <- geog$routing_zone_centroids %||% geog$analysis_zone_centroids
  centroids <- centroids %>% dplyr::mutate(zone_id = standardize_zone_id(zone_id, cfg$geography$analysis_unit)) %>% sf::st_transform(4326)
  coords <- tryCatch(sf::st_coordinates(centroids), error = function(e) matrix(NA_real_, nrow(centroids), 2))
  if (nrow(coords) != nrow(centroids)) coords <- matrix(NA_real_, nrow(centroids), 2)
  qc <- centroids %>% sf::st_drop_geometry() %>% dplyr::transmute(
    zone_id = as.character(zone_id),
    lon = suppressWarnings(as.numeric(coords[, 1])),
    lat = suppressWarnings(as.numeric(coords[, 2]))
  )
  qc %>% dplyr::mutate(coord_valid = !is.na(lon) & !is.na(lat) & is.finite(lon) & is.finite(lat) & dplyr::between(lon, -180, 180) & dplyr::between(lat, -90, 90)) %>% dplyr::distinct(zone_id, .keep_all = TRUE)
}

build_point_df <- function(zone_ids, qc_tbl) {
  qc_tbl %>% dplyr::filter(zone_id %in% zone_ids, coord_valid) %>% dplyr::transmute(id = zone_id, lon = as.numeric(lon), lat = as.numeric(lat)) %>% dplyr::distinct(id, .keep_all = TRUE)
}

choose_test_pairs <- function(cfg, qc_tbl, pairs_csv_arg = NULL) {
  valid_ids <- unique(qc_tbl$zone_id[qc_tbl$coord_valid])
  pairs <- tibble::tibble()
  add_pair <- function(df, o, d, label, source) {
    dplyr::bind_rows(df, tibble::tibble(origin_id = o, destination_id = d, pair_label = label, pair_source = source))
  }
  if (!is.null(pairs_csv_arg) && file.exists(pairs_csv_arg)) {
    user_pairs <- read_csv_guess(pairs_csv_arg)
    req <- c("origin_id", "destination_id")
    if (!all(req %in% names(user_pairs))) stop("pairs_csv must contain origin_id and destination_id columns", call. = FALSE)
    if (!"pair_label" %in% names(user_pairs)) user_pairs$pair_label <- paste0("user_pair_", seq_len(nrow(user_pairs)))
    user_pairs <- user_pairs %>% dplyr::transmute(origin_id = standardize_zone_id(origin_id, cfg$geography$analysis_unit), destination_id = standardize_zone_id(destination_id, cfg$geography$analysis_unit), pair_label, pair_source = "user_pairs")
    pairs <- dplyr::bind_rows(pairs, user_pairs)
  }
  known_pairs <- tibble::tribble(
    ~origin_id, ~destination_id, ~pair_label,
    "02108", "02139", "boston_core_to_cambridge",
    "02108", "02109", "downtown_adjacent",
    "02115", "02139", "fenway_to_cambridge",
    "02116", "02139", "backbay_to_cambridge",
    "02445", "02139", "brookline_to_cambridge",
    "02108", "02108", "downtown_self"
  ) %>% dplyr::filter(origin_id %in% valid_ids, destination_id %in% valid_ids) %>% dplyr::mutate(pair_source = "known_pairs")
  pairs <- dplyr::bind_rows(pairs, known_pairs)
  pairs <- pairs %>% dplyr::mutate(origin_id = standardize_zone_id(origin_id, cfg$geography$analysis_unit), destination_id = standardize_zone_id(destination_id, cfg$geography$analysis_unit)) %>% dplyr::filter(origin_id %in% valid_ids, destination_id %in% valid_ids) %>% dplyr::distinct(origin_id, destination_id, .keep_all = TRUE)
  if (is.null(pairs_csv_arg) || !file.exists(pairs_csv_arg)) {
    pairs <- pairs %>% dplyr::slice_head(n = 4)
  }
  pairs %>% dplyr::mutate(pair_id = paste0(origin_id, "__", destination_id))
}

run_ttm_direct <- function(network, origins_df, destinations_df, mode_vec, departure_datetime, time_window_minutes, cfg) {
  r5r::travel_time_matrix(
    r5r_network = network,
    origins = origins_df,
    destinations = destinations_df,
    mode = mode_vec,
    departure_datetime = departure_datetime,
    time_window = as.integer(time_window_minutes),
    percentiles = unlist(cfg$routing$percentiles),
    max_walk_time = cfg$routing$max_walk_time,
    max_trip_duration = cfg$routing$max_trip_duration,
    walk_speed = cfg$routing$walk_speed,
    max_rides = cfg$routing$max_rides,
    n_threads = 1L,
    progress = FALSE
  )
}

normalize_raw_ttm <- function(x, cfg) {
  if (is.null(x)) return(tibble::tibble())
  out <- tibble::as_tibble(x)
  if (nrow(out) == 0) return(out)
  tt_cols <- grep("^travel_time_p[0-9]+$", names(out), value = TRUE)
  out <- out %>% dplyr::mutate(from_id = standardize_zone_id(as.character(from_id), cfg$geography$analysis_unit), to_id = standardize_zone_id(as.character(to_id), cfg$geography$analysis_unit))
  if (length(tt_cols) > 0) out[tt_cols] <- lapply(out[tt_cols], function(col) suppressWarnings(as.numeric(col)))
  out
}

inspect_routes_detailed <- function(gtfs_zip_path) {
  routes <- read_zip_csv(gtfs_zip_path, "routes.txt")
  trips <- read_zip_csv(gtfs_zip_path, "trips.txt")
  stop_times <- read_zip_csv(gtfs_zip_path, "stop_times.txt")
  if (is.null(routes)) return(tibble::tibble())
  if (!"route_type" %in% names(routes)) routes$route_type <- NA_character_
  routes <- routes %>% dplyr::mutate(route_type_num = suppressWarnings(as.integer(route_type)))
  trip_counts <- if (!is.null(trips) && "route_id" %in% names(trips)) trips %>% dplyr::count(route_id, name = "n_trips") else tibble::tibble(route_id = character(), n_trips = integer())
  st_counts <- if (!is.null(stop_times) && !is.null(trips) && all(c("trip_id", "route_id") %in% names(trips)) && "trip_id" %in% names(stop_times)) {
    stop_times %>% dplyr::count(trip_id, name = "n_stop_times") %>% dplyr::right_join(trips %>% dplyr::select(trip_id, route_id), by = "trip_id") %>% dplyr::group_by(route_id) %>% dplyr::summarise(n_stop_times = sum(n_stop_times, na.rm = TRUE), .groups = "drop")
  } else tibble::tibble(route_id = character(), n_stop_times = integer())
  routes %>% dplyr::left_join(trip_counts, by = "route_id") %>% dplyr::left_join(st_counts, by = "route_id") %>% dplyr::mutate(n_trips = dplyr::coalesce(n_trips, 0L), n_stop_times = dplyr::coalesce(n_stop_times, 0L))
}

inspect_canonical_hypothesis <- function(gtfs_zip_path) {
  trips <- read_zip_csv(gtfs_zip_path, "trips.txt")
  stop_times <- read_zip_csv(gtfs_zip_path, "stop_times.txt")
  calendar <- read_zip_csv(gtfs_zip_path, "calendar.txt")
  calendar_dates <- read_zip_csv(gtfs_zip_path, "calendar_dates.txt")
  route_patterns <- read_zip_csv(gtfs_zip_path, "route_patterns.txt")
  routes <- inspect_routes_detailed(gtfs_zip_path)
  if (is.null(trips) || is.null(stop_times)) {
    return(list(summary = tibble::tibble(), mixed_patterns = tibble::tibble(), canonical_trips = tibble::tibble(), routes = routes))
  }
  trips <- trips %>% dplyr::mutate(
    is_canonical_trip = grepl("^canonical-", trip_id %||% "") | (service_id %||% "") == "canonical",
    route_type_num = suppressWarnings(as.integer(trip_route_type))
  )
  if (!"route_pattern_id" %in% names(trips)) trips$route_pattern_id <- NA_character_
  if (!"stop_sequence" %in% names(stop_times)) stop_times$stop_sequence <- NA_character_
  st_templates <- stop_times %>%
    dplyr::mutate(stop_sequence_num = suppressWarnings(as.integer(stop_sequence))) %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(
      n_stops = dplyr::n(),
      seq_template = paste(stop_sequence_num, collapse = ";"),
      .groups = "drop"
    )
  trip_templates <- trips %>% dplyr::left_join(st_templates, by = "trip_id")
  mixed_patterns <- trip_templates %>%
    dplyr::filter(!is.na(route_pattern_id)) %>%
    dplyr::group_by(route_id, route_pattern_id) %>%
    dplyr::summarise(
      n_trips = dplyr::n(),
      n_canonical_trips = sum(is_canonical_trip, na.rm = TRUE),
      n_noncanonical_trips = sum(!is_canonical_trip, na.rm = TRUE),
      n_distinct_seq_templates = dplyr::n_distinct(seq_template, na.rm = TRUE),
      canonical_templates = paste(sort(unique(seq_template[is_canonical_trip & !is.na(seq_template)])), collapse = " | "),
      noncanonical_templates = paste(sort(unique(seq_template[!is_canonical_trip & !is.na(seq_template)])), collapse = " | "),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_canonical_trips > 0, n_noncanonical_trips > 0)
  summary <- tibble::tibble(
    n_routes_total = if (nrow(routes) > 0) nrow(routes) else NA_integer_,
    n_rail_routes = if (nrow(routes) > 0) sum(routes$route_type_num == 2L, na.rm = TRUE) else NA_integer_,
    n_trips_total = nrow(trips),
    n_canonical_trips = sum(trips$is_canonical_trip, na.rm = TRUE),
    n_canonical_service_rows = if (!is.null(calendar) && "service_id" %in% names(calendar)) sum(calendar$service_id == "canonical", na.rm = TRUE) else 0L,
    n_canonical_calendar_dates = if (!is.null(calendar_dates) && "service_id" %in% names(calendar_dates)) sum(calendar_dates$service_id == "canonical", na.rm = TRUE) else 0L,
    n_patterns_mixed_canonical = nrow(mixed_patterns),
    n_patterns_mixed_canonical_and_multiple_templates = sum(mixed_patterns$n_distinct_seq_templates > 1L, na.rm = TRUE)
  )
  canonical_trips <- trip_templates %>% dplyr::filter(is_canonical_trip) %>% dplyr::select(route_id, service_id, trip_id, route_pattern_id, n_stops, seq_template)
  if (!is.null(route_patterns) && "route_pattern_id" %in% names(route_patterns)) {
    mixed_patterns <- mixed_patterns %>% dplyr::left_join(route_patterns %>% dplyr::select(route_pattern_id, route_id, dplyr::any_of(c("direction_id", "representative_trip_id", "typicality"))) %>% dplyr::distinct(), by = c("route_pattern_id", "route_id"))
  }
  list(summary = summary, mixed_patterns = mixed_patterns, canonical_trips = canonical_trips, routes = routes)
}

write_gtfs_table <- function(df, path) readr::write_csv(df, path, na = "")

create_variant_from_base <- function(base_zip_path, variant_label, out_dir, route_id_arg = NULL, drop_canonical = FALSE, renumber_stop_sequence = FALSE) {
  td <- tempfile(paste0("canonical_variant_", variant_label, "_"))
  fs::dir_create(td)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(base_zip_path, exdir = td)

  read_txt <- function(name) {
    p <- file.path(td, name)
    if (!file.exists(p)) return(NULL)
    readr::read_csv(p, show_col_types = FALSE, progress = FALSE, col_types = readr::cols(.default = "c"))
  }

  routes <- read_txt("routes.txt")
  trips <- read_txt("trips.txt")
  stop_times <- read_txt("stop_times.txt")
  stops <- read_txt("stops.txt")
  calendar <- read_txt("calendar.txt")
  calendar_dates <- read_txt("calendar_dates.txt")
  shapes <- read_txt("shapes.txt")
  route_patterns <- read_txt("route_patterns.txt")
  frequencies <- read_txt("frequencies.txt")
  transfers <- read_txt("transfers.txt")
  pathways <- read_txt("pathways.txt")
  fare_rules <- read_txt("fare_rules.txt")

  if (is.null(routes) || is.null(trips) || is.null(stop_times) || is.null(stops)) {
    return(list(status = "missing_required_tables", variant_label = variant_label, zip_path = NA_character_))
  }
  if (!"route_type" %in% names(routes)) routes$route_type <- NA_character_
  routes <- routes %>% dplyr::mutate(route_type_num = suppressWarnings(as.integer(route_type)))

  if (!is.null(route_id_arg) && nzchar(route_id_arg)) {
    keep_route_ids <- routes %>% dplyr::filter(route_type_num %in% c(0L, 1L, 3L) | route_id == route_id_arg) %>% dplyr::pull(route_id) %>% unique()
    routes <- routes %>% dplyr::filter(route_id %in% keep_route_ids)
    trips <- trips %>% dplyr::filter(route_id %in% keep_route_ids)
  }

  trips <- trips %>% dplyr::mutate(is_canonical_trip = grepl("^canonical-", trip_id %||% "") | (service_id %||% "") == "canonical")
  if (drop_canonical) {
    trips <- trips %>% dplyr::filter(!is_canonical_trip)
    if (!is.null(calendar) && "service_id" %in% names(calendar)) calendar <- calendar %>% dplyr::filter(service_id != "canonical")
    if (!is.null(calendar_dates) && "service_id" %in% names(calendar_dates)) calendar_dates <- calendar_dates %>% dplyr::filter(service_id != "canonical")
  }

  trip_ids <- unique(trips$trip_id)
  stop_times <- stop_times %>% dplyr::filter(trip_id %in% trip_ids)
  if (!is.null(frequencies) && "trip_id" %in% names(frequencies)) frequencies <- frequencies %>% dplyr::filter(trip_id %in% trip_ids)
  if (renumber_stop_sequence && "stop_sequence" %in% names(stop_times)) {
    stop_times <- stop_times %>%
      dplyr::mutate(stop_sequence_num = suppressWarnings(as.integer(stop_sequence))) %>%
      dplyr::group_by(trip_id) %>%
      dplyr::arrange(stop_sequence_num, .by_group = TRUE) %>%
      dplyr::mutate(stop_sequence = as.character(seq_len(dplyr::n()))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-dplyr::any_of("stop_sequence_num"))
  }

  stop_ids <- unique(stop_times$stop_id)
  stops <- stops %>% dplyr::filter(stop_id %in% stop_ids)
  if (!is.null(shapes) && "shape_id" %in% names(shapes) && "shape_id" %in% names(trips)) shapes <- shapes %>% dplyr::filter(shape_id %in% unique(stats::na.omit(trips$shape_id)))
  if (!is.null(route_patterns) && "route_pattern_id" %in% names(route_patterns) && "route_pattern_id" %in% names(trips)) route_patterns <- route_patterns %>% dplyr::filter(route_pattern_id %in% unique(stats::na.omit(trips$route_pattern_id)))
  if (!is.null(transfers)) {
    if ("from_stop_id" %in% names(transfers)) transfers <- transfers %>% dplyr::filter(is.na(from_stop_id) | from_stop_id %in% stop_ids)
    if ("to_stop_id" %in% names(transfers)) transfers <- transfers %>% dplyr::filter(is.na(to_stop_id) | to_stop_id %in% stop_ids)
    if ("from_trip_id" %in% names(transfers)) transfers <- transfers %>% dplyr::filter(is.na(from_trip_id) | from_trip_id %in% trip_ids)
    if ("to_trip_id" %in% names(transfers)) transfers <- transfers %>% dplyr::filter(is.na(to_trip_id) | to_trip_id %in% trip_ids)
  }
  if (!is.null(pathways)) {
    if ("from_stop_id" %in% names(pathways)) pathways <- pathways %>% dplyr::filter(is.na(from_stop_id) | from_stop_id %in% stop_ids)
    if ("to_stop_id" %in% names(pathways)) pathways <- pathways %>% dplyr::filter(is.na(to_stop_id) | to_stop_id %in% stop_ids)
  }
  if (!is.null(fare_rules) && "route_id" %in% names(fare_rules)) fare_rules <- fare_rules %>% dplyr::filter(is.na(route_id) | route_id %in% unique(trips$route_id))

  # drop helper columns from write-out
  trips <- trips %>% dplyr::select(-dplyr::any_of("is_canonical_trip"))
  routes <- routes %>% dplyr::select(-dplyr::any_of("route_type_num"))

  write_gtfs_table(routes, file.path(td, "routes.txt"))
  write_gtfs_table(trips, file.path(td, "trips.txt"))
  write_gtfs_table(stop_times, file.path(td, "stop_times.txt"))
  write_gtfs_table(stops, file.path(td, "stops.txt"))
  if (!is.null(calendar)) write_gtfs_table(calendar, file.path(td, "calendar.txt"))
  if (!is.null(calendar_dates)) write_gtfs_table(calendar_dates, file.path(td, "calendar_dates.txt"))
  if (!is.null(shapes)) write_gtfs_table(shapes, file.path(td, "shapes.txt"))
  if (!is.null(route_patterns)) write_gtfs_table(route_patterns, file.path(td, "route_patterns.txt"))
  if (!is.null(frequencies)) write_gtfs_table(frequencies, file.path(td, "frequencies.txt"))
  if (!is.null(transfers)) write_gtfs_table(transfers, file.path(td, "transfers.txt"))
  if (!is.null(pathways)) write_gtfs_table(pathways, file.path(td, "pathways.txt"))
  if (!is.null(fare_rules)) write_gtfs_table(fare_rules, file.path(td, "fare_rules.txt"))

  out_zip <- file.path(out_dir, paste0("gtfs_variant_", variant_label, ".zip"))
  zip_dir_flat(td, out_zip)
  list(
    status = "ok",
    variant_label = variant_label,
    zip_path = out_zip,
    n_routes = nrow(routes),
    n_trips = nrow(trips),
    n_stop_times = nrow(stop_times),
    n_stops = nrow(stops),
    dropped_canonical = isTRUE(drop_canonical),
    renumbered_stop_sequence = isTRUE(renumber_stop_sequence)
  )
}

probe_variant <- function(cfg, context, variant_zip, variant_label, pair_manifest, origins_unique, destinations_unique, departure_datetime, time_window_minutes, out_dir) {
  cfg_variant <- cfg
  cfg_variant$osm$local_pbf_path <- resolve_base_network_osm_pbf(cfg, context$feed_name)
  build <- safe_run(build_r5_network_object(cfg_variant, paste0(context$feed_name, "__", variant_label), list(variant_zip)))
  sitrep_path <- file.path(out_dir, paste0("r5r_sitrep_", variant_label, ".txt"))
  if (identical(build$status, "ok")) capture_r5r_sitrep(sitrep_path)
  build_row <- tibble::tibble(
    variant_label = variant_label,
    status = build$status,
    error_message = build$error_message,
    network_built = identical(build$status, "ok")
  )
  if (!identical(build$status, "ok")) {
    return(list(build = build_row, summary = tibble::tibble(variant_label = variant_label, runner = "direct", mode_label = c("walk_only", "transit_with_rail"), status = "build_error", error_message = build$error_message, n_rows_returned = 0L, n_exact_requested_pairs_hit = 0L, n_self_pairs_returned = 0L), raw = tibble::tibble(), hits = tibble::tibble()))
  }
  network <- build$result
  on.exit(try({ network$stop(); safe_message("variant_network has been successfully stopped.") }, silent = TRUE), add = TRUE)
  matrix_runs <- list(
    walk_only = c("WALK"),
    transit_with_rail = c("WALK", "TRANSIT")
  )
  summary <- tibble::tibble()
  raw_all <- tibble::tibble()
  for (nm in names(matrix_runs)) {
    probe <- safe_run(run_ttm_direct(network, origins_unique, destinations_unique, matrix_runs[[nm]], departure_datetime, time_window_minutes, cfg))
    raw <- if (identical(probe$status, "ok")) normalize_raw_ttm(probe$result, cfg) else tibble::tibble()
    if (nrow(raw) > 0) raw <- raw %>% dplyr::mutate(variant_label = variant_label, runner = "direct", mode_label = nm)
    raw_all <- dplyr::bind_rows(raw_all, raw)
    summary <- dplyr::bind_rows(summary, tibble::tibble(
      variant_label = variant_label,
      runner = "direct",
      mode_label = nm,
      status = probe$status,
      error_message = probe$error_message,
      n_rows_returned = nrow(raw),
      n_exact_requested_pairs_hit = if (nrow(raw) > 0) sum(paste0(raw$from_id, "__", raw$to_id) %in% pair_manifest$pair_id) else 0L,
      n_self_pairs_returned = if (nrow(raw) > 0) sum(raw$from_id == raw$to_id, na.rm = TRUE) else 0L
    ))
  }
  hits <- if (nrow(raw_all) > 0) {
    pair_manifest %>%
      dplyr::cross_join(summary %>% dplyr::select(variant_label, runner, mode_label)) %>%
      dplyr::left_join(raw_all %>% dplyr::mutate(pair_id = paste0(from_id, "__", to_id)) %>% dplyr::count(variant_label, runner, mode_label, pair_id, name = "raw_rows_for_pair"), by = c("variant_label", "runner", "mode_label", "pair_id")) %>%
      dplyr::mutate(exact_pair_hit = !is.na(raw_rows_for_pair) & raw_rows_for_pair > 0L)
  } else {
    pair_manifest %>% dplyr::cross_join(summary %>% dplyr::select(variant_label, runner, mode_label)) %>% dplyr::mutate(raw_rows_for_pair = 0L, exact_pair_hit = FALSE)
  }
  list(build = build_row, summary = summary, raw = raw_all, hits = hits)
}

safe_message <- function(...) cat(paste0(..., "\n"))

context <- resolve_context(cfg, feed_name_arg, analysis_date_arg, time_window_id_arg)
base_gtfs_path <- resolve_gtfs_base(cfg, context, gtfs_zip_override_arg)
base_osm_path <- resolve_base_network_osm_pbf(cfg, context$feed_name)
write_json_pretty(list(config_path = config_path, project_root = project_root, feed_name = context$feed_name, analysis_date = as.character(context$analysis_date), time_window_id = context$time_window_id, route_id = route_id_arg, base_gtfs_path = base_gtfs_path, base_osm_path = base_osm_path), file.path(out_dir, "probe_context.json"))

base_diag <- inspect_canonical_hypothesis(base_gtfs_path)
readr::write_csv(base_diag$summary, file.path(out_dir, "canonical_hypothesis_summary_base.csv"))
readr::write_csv(base_diag$mixed_patterns, file.path(out_dir, "canonical_hypothesis_mixed_patterns_base.csv"))
readr::write_csv(base_diag$canonical_trips, file.path(out_dir, "canonical_hypothesis_canonical_trips_base.csv"))
readr::write_csv(base_diag$routes, file.path(out_dir, "gtfs_route_inventory_detailed_base.csv"))

# If a route_id is supplied and the base GTFS is not already an override of one line, build a targeted base.
base_variant_label <- if (!is.null(route_id_arg) && nzchar(route_id_arg) && is.null(gtfs_zip_override_arg)) paste0("target_", gsub("[^A-Za-z0-9]+", "_", route_id_arg)) else "base"
base_variant <- if (!is.null(route_id_arg) && nzchar(route_id_arg) && is.null(gtfs_zip_override_arg)) {
  create_variant_from_base(base_gtfs_path, base_variant_label, out_dir, route_id_arg = route_id_arg, drop_canonical = FALSE, renumber_stop_sequence = FALSE)
} else {
  list(status = "ok", variant_label = base_variant_label, zip_path = base_gtfs_path)
}

variants_meta <- tibble::tibble(
  variant_label = character(),
  status = character(),
  zip_path = character(),
  drop_canonical = logical(),
  renumber_stop_sequence = logical(),
  n_routes = integer(),
  n_trips = integer(),
  n_stop_times = integer(),
  n_stops = integer()
)

collect_variant_meta <- function(v, drop_canonical, renumber_stop_sequence) {
  tibble::tibble(
    variant_label = as.character(v$variant_label %||% NA_character_),
    status = as.character(v$status %||% NA_character_),
    zip_path = as.character(v$zip_path %||% NA_character_),
    drop_canonical = isTRUE(drop_canonical),
    renumber_stop_sequence = isTRUE(renumber_stop_sequence),
    n_routes = suppressWarnings(as.integer(v$n_routes %||% NA_integer_)),
    n_trips = suppressWarnings(as.integer(v$n_trips %||% NA_integer_)),
    n_stop_times = suppressWarnings(as.integer(v$n_stop_times %||% NA_integer_)),
    n_stops = suppressWarnings(as.integer(v$n_stops %||% NA_integer_))
  )
}
variants_meta <- dplyr::bind_rows(variants_meta, collect_variant_meta(base_variant, FALSE, FALSE))
if (!identical(base_variant$status, "ok") || is.na(base_variant$zip_path[[1]]) || !file.exists(base_variant$zip_path[[1]])) stop("Failed to create targeted base GTFS variant.", call. = FALSE)

no_canonical_variant <- create_variant_from_base(base_variant$zip_path[[1]], paste0(base_variant_label, "__drop_canonical"), out_dir, route_id_arg = NULL, drop_canonical = TRUE, renumber_stop_sequence = FALSE)
variants_meta <- dplyr::bind_rows(variants_meta, collect_variant_meta(no_canonical_variant, TRUE, FALSE))

no_canonical_renumber_variant <- create_variant_from_base(base_variant$zip_path[[1]], paste0(base_variant_label, "__drop_canonical_renumber"), out_dir, route_id_arg = NULL, drop_canonical = TRUE, renumber_stop_sequence = TRUE)
variants_meta <- dplyr::bind_rows(variants_meta, collect_variant_meta(no_canonical_renumber_variant, TRUE, TRUE))
readr::write_csv(variants_meta, file.path(out_dir, "variant_build_plan.csv"))

# Inspect each written GTFS variant.
variant_diags <- purrr::map_dfr(variants_meta$zip_path[file.exists(variants_meta$zip_path)], function(zp) {
  diag <- inspect_canonical_hypothesis(zp)
  summary <- diag$summary
  if (nrow(summary) == 0) summary <- tibble::tibble()
  summary$variant_label <- tools::file_path_sans_ext(basename(zp))
  summary
})
readr::write_csv(variant_diags, file.path(out_dir, "canonical_hypothesis_summary_variants.csv"))

# Build geography and points once from the current project config.
geog_try <- safe_run(read_geography_outputs(cfg))
if (identical(geog_try$status, "ok")) {
  geog <- geog_try$result
} else {
  geog_build_try <- safe_run(download_county_tract_geography(cfg))
  if (!identical(geog_build_try$status, "ok")) {
    stop("Failed to load or build geography outputs: ", geog_build_try$error_message %||% geog_try$error_message, call. = FALSE)
  }
  geog <- geog_build_try$result
}
qc_tbl <- build_zone_point_qc(geog, cfg)
readr::write_csv(qc_tbl, file.path(out_dir, "zone_point_qc_probe.csv"))
pair_manifest <- choose_test_pairs(cfg, qc_tbl, pairs_csv_arg)
readr::write_csv(pair_manifest, file.path(out_dir, "test_pair_manifest.csv"))
origins_unique <- build_point_df(unique(pair_manifest$origin_id), qc_tbl)
destinations_unique <- build_point_df(unique(pair_manifest$destination_id), qc_tbl)
readr::write_csv(origins_unique, file.path(out_dir, "probe_origins_points.csv"))
readr::write_csv(destinations_unique, file.path(out_dir, "probe_destinations_points.csv"))

# Runtime context
selected_dates <- choose_routing_dates(cfg)
readr::write_csv(selected_dates, file.path(out_dir, "routing_dates_selected.csv"))
service_levels <- service_levels_for_context(cfg, context$feed_name, context$analysis_date)
if (nrow(service_levels) > 0) readr::write_csv(service_levels, file.path(out_dir, "service_levels_for_probe_date.csv"))

# Probe settings
probe_time_window_minutes <- 1L
probe_date_time <- as.POSIXct(sprintf("%s 08:00:00", as.character(context$analysis_date)), tz = cfg$project$timezone %||% "America/New_York")
write_json_pretty(list(departure_datetime = as.character(probe_date_time), probe_time_window_minutes = probe_time_window_minutes, route_id = route_id_arg, base_gtfs_path = base_gtfs_path, base_variant_label = base_variant_label), file.path(out_dir, "routing_runtime_snapshot.json"))

# Run build and minimal probes for each variant.
variant_results <- purrr::map(variants_meta$variant_label, function(vlab) {
  row <- variants_meta %>% dplyr::filter(variant_label == !!vlab) %>% dplyr::slice(1)
  if (nrow(row) == 0 || is.na(row$zip_path[[1]]) || !file.exists(row$zip_path[[1]]) || row$status[[1]] != "ok") {
    return(list(build = tibble::tibble(variant_label = vlab, status = "variant_missing", error_message = "variant zip missing", network_built = FALSE), summary = tibble::tibble(), raw = tibble::tibble(), hits = tibble::tibble()))
  }
  probe_variant(cfg, context, row$zip_path[[1]], vlab, pair_manifest, origins_unique, destinations_unique, probe_date_time, probe_time_window_minutes, out_dir)
})

build_summary <- purrr::map_dfr(variant_results, "build")
probe_summary <- purrr::map_dfr(variant_results, "summary")
probe_raw <- purrr::map_dfr(variant_results, "raw")
probe_hits <- purrr::map_dfr(variant_results, "hits")
readr::write_csv(build_summary, file.path(out_dir, "canonical_probe_build_summary.csv"))
readr::write_csv(probe_summary, file.path(out_dir, "canonical_probe_matrix_summary.csv"))
readr::write_csv(probe_raw, file.path(out_dir, "canonical_probe_matrix_raw.csv"))
readr::write_csv(probe_hits, file.path(out_dir, "canonical_probe_requested_pair_hits.csv"))

# Markdown report
base_mixed_n <- if (nrow(base_diag$summary) > 0) as.integer(base_diag$summary$n_patterns_mixed_canonical_and_multiple_templates[[1]] %||% 0L) else 0L
base_can_trips <- if (nrow(base_diag$summary) > 0) as.integer(base_diag$summary$n_canonical_trips[[1]] %||% 0L) else 0L
base_rail_routes <- if (nrow(base_diag$summary) > 0) as.integer(base_diag$summary$n_rail_routes[[1]] %||% NA_integer_) else NA_integer_
md <- c(
  "# Canonical commuter-rail hypothesis probe",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("Feed: `", context$feed_name, "`"),
  paste0("Analysis date: `", as.character(context$analysis_date), "`"),
  paste0("Time window id: `", context$time_window_id, "`"),
  paste0("Route id target: `", route_id_arg %||% "(none / use provided GTFS override)", "`"),
  "",
  "## Base GTFS diagnostics",
  paste0("- Rail routes in feed: `", base_rail_routes, "`"),
  paste0("- Canonical trips in base GTFS: `", base_can_trips, "`"),
  paste0("- Mixed route patterns with >1 stop-sequence template across canonical and non-canonical trips: `", base_mixed_n, "`"),
  "",
  "## Variants tested",
  "- base or targeted base",
  "- drop canonical trips and `service_id == canonical`",
  "- drop canonical + renumber `stop_sequence` contiguously per trip",
  "",
  "## Probe interpretation",
  "- If `base` crashes on `transit_with_rail` but `drop_canonical` succeeds, the canonical-trip hypothesis is strongly supported.",
  "- If `drop_canonical` still crashes but `drop_canonical_renumber` succeeds, stop-sequence numbering is the stronger trigger.",
  "- If all three variants still crash, the cause is likely a broader commuter-rail pattern issue not limited to canonical trips.",
  "",
  "## Key outputs",
  "- `canonical_hypothesis_summary_base.csv`",
  "- `canonical_hypothesis_mixed_patterns_base.csv`",
  "- `variant_build_plan.csv`",
  "- `canonical_probe_build_summary.csv`",
  "- `canonical_probe_matrix_summary.csv`",
  "- `canonical_probe_requested_pair_hits.csv`"
)
save_text(md, file.path(out_dir, "probe_report.md"))

capture_r5r_sitrep(file.path(out_dir, "r5r_sitrep_global.txt"))
cat("Wrote canonical commuter-rail diagnostics to:", out_dir, "\n")
print(probe_summary)
