#!/usr/bin/env Rscript

# Minimal R5/r5r probe script for debugging routing failures.
# Usage:
#   Rscript scripts/minimal_r5_probe_diagnostics.R <config_path> [source_id] [analysis_unit] [feed_name] [analysis_date] [time_window_id] [pairs_csv]
#
# Outputs a timestamped diagnostics folder under cfg$paths$logs_dir with:
#   - context and config snapshots
#   - selected routing dates and service levels
#   - robust point QC for all candidate test points
#   - failure log summaries from prior runs
#   - optional snap diagnostics
#   - raw single-pair probe outputs for WALK and configured transit modes
#   - raw small-matrix probe outputs for direct r5r and project wrapper
#   - exact-pair hit checks to see whether returned rows match requested OD pairs

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
source_id_arg <- arg_or_null(2)
analysis_unit_arg <- arg_or_null(3)
feed_name_arg <- arg_or_null(4)
analysis_date_arg <- arg_or_null(5)
time_window_id_arg <- arg_or_null(6)
pairs_csv_arg <- arg_or_null(7)
if (!is.null(pairs_csv_arg) && !grepl("^(~|/|[A-Za-z]:)", pairs_csv_arg)) pairs_csv_arg <- file.path(project_root, pairs_csv_arg)

source(file.path(project_root, "R", "packages.R"))
java_boot <- peek_java_config(config_path)
configure_java_for_r5r(java_memory = java_boot$java_memory, java_active_processors = java_boot$java_active_processors, force = TRUE)
source(file.path(project_root, "R", "load_project.R"))
load_project(project_root)

cfg <- load_project_config(config_path, source_id_arg)
if (!is.null(analysis_unit_arg)) cfg <- apply_runtime_overrides(cfg, list(analysis_unit = analysis_unit_arg))
ensure_project_dirs(cfg)

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")
out_dir <- file.path(cfg$paths$logs_dir, paste0("minimal_r5_probe_", stamp))
fs::dir_create(out_dir)

write_json_pretty <- function(x, path) jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE, null = "null")

safe_message <- function(...) cat(paste0(..., "\n"))
save_text <- function(txt, path) writeLines(as.character(txt), path, useBytes = TRUE)

extract_failure_contexts <- function(log_dir) {
  empty_tbl <- tibble::tibble(
    log_file = character(),
    feed_name = character(),
    analysis_date = character(),
    time_window_id = character(),
    od_scenario_id = character(),
    origin_chunk_id = integer(),
    destination_chunk_id = integer(),
    initial_error = character(),
    retry_error = character(),
    n_failed_origins = integer(),
    failed_origins = character(),
    path = character()
  )
  files <- if (dir.exists(log_dir)) fs::dir_ls(log_dir, glob = "routing_origin_fallback_*.json") else character()
  if (length(files) == 0) return(empty_tbl)
  out <- purrr::map_dfr(files, function(f) {
    j <- tryCatch(jsonlite::read_json(f, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(j)) return(tibble::tibble())
    failed_origins <- j$failed_origins
    n_failed <- 0L
    origin_ids <- character()
    if (is.data.frame(failed_origins) && "origin_id" %in% names(failed_origins)) {
      origin_ids <- as.character(failed_origins$origin_id)
      n_failed <- nrow(failed_origins)
    } else if (is.list(failed_origins) && length(failed_origins) > 0) {
      origin_ids <- purrr::map_chr(failed_origins, ~ as.character(.x$origin_id %||% NA_character_))
      origin_ids <- origin_ids[!is.na(origin_ids)]
      n_failed <- length(origin_ids)
    }
    tibble::tibble(
      log_file = basename(f),
      feed_name = as.character(j$feed_name %||% NA_character_),
      analysis_date = as.character(j$analysis_date %||% NA_character_),
      time_window_id = as.character(j$time_window_id %||% NA_character_),
      od_scenario_id = as.character(j$od_scenario_id %||% NA_character_),
      origin_chunk_id = suppressWarnings(as.integer(j$origin_chunk_id %||% NA_integer_)),
      destination_chunk_id = suppressWarnings(as.integer(j$destination_chunk_id %||% NA_integer_)),
      initial_error = as.character(j$initial_error %||% NA_character_),
      retry_error = as.character(j$retry_error %||% NA_character_),
      n_failed_origins = as.integer(n_failed),
      failed_origins = paste(unique(origin_ids), collapse = ";"),
      path = f
    )
  })
  if (nrow(out) == 0) return(empty_tbl)
  out
}

build_zone_point_qc <- function(geog, cfg) {
  centroids <- geog$routing_zone_centroids %||% geog$analysis_zone_centroids
  zones <- geog$analysis_zones %>%
    dplyr::mutate(zone_id = standardize_zone_id(zone_id, cfg$geography$analysis_unit)) %>%
    sf::st_transform(4326)
  centroids <- centroids %>%
    dplyr::mutate(zone_id = standardize_zone_id(zone_id, cfg$geography$analysis_unit)) %>%
    sf::st_transform(4326)

  coords <- tryCatch(sf::st_coordinates(centroids), error = function(e) matrix(NA_real_, nrow(centroids), 2))
  if (nrow(coords) != nrow(centroids)) coords <- matrix(NA_real_, nrow(centroids), 2)

  cent_drop <- centroids %>% sf::st_drop_geometry()
  point_method_vec <- if ("point_method" %in% names(cent_drop)) as.character(cent_drop$point_method) else rep("routing_centroid_geometry", nrow(cent_drop))
  point_method_vec[is.na(point_method_vec) | !nzchar(point_method_vec)] <- "routing_centroid_geometry"
  rep_tract_vec <- if ("representative_tract_id" %in% names(cent_drop)) as.character(cent_drop$representative_tract_id) else rep(NA_character_, nrow(cent_drop))

  qc <- cent_drop %>%
    dplyr::transmute(
      zone_id = as.character(zone_id),
      lon = suppressWarnings(as.numeric(coords[, 1])),
      lat = suppressWarnings(as.numeric(coords[, 2])),
      point_source = point_method_vec,
      representative_tract_id = rep_tract_vec
    )

  bad <- is.na(qc$lon) | is.na(qc$lat) | !is.finite(qc$lon) | !is.finite(qc$lat) |
    qc$lon < -180 | qc$lon > 180 | qc$lat < -90 | qc$lat > 90

  if (any(bad)) {
    fallback_zones <- zones %>% dplyr::filter(zone_id %in% qc$zone_id[bad])
    if (nrow(fallback_zones) > 0) {
      fallback_pts <- fallback_zones %>% sf::st_make_valid()
      fallback_geom <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(fallback_pts)))
      sf::st_geometry(fallback_pts) <- fallback_geom
      fallback_coords <- tryCatch(sf::st_coordinates(fallback_pts), error = function(e) matrix(NA_real_, nrow(fallback_pts), 2))
      fb <- tibble::tibble(
        zone_id = as.character(fallback_pts$zone_id),
        lon_fb = suppressWarnings(as.numeric(fallback_coords[, 1])),
        lat_fb = suppressWarnings(as.numeric(fallback_coords[, 2]))
      )
      qc <- qc %>% dplyr::left_join(fb, by = "zone_id") %>%
        dplyr::mutate(
          use_fb = (is.na(lon) | is.na(lat) | !is.finite(lon) | !is.finite(lat)) & !is.na(lon_fb) & !is.na(lat_fb),
          lon = dplyr::if_else(use_fb, lon_fb, lon),
          lat = dplyr::if_else(use_fb, lat_fb, lat),
          point_source = dplyr::if_else(use_fb, "fallback_zone_point_on_surface", point_source)
        ) %>%
        dplyr::select(-lon_fb, -lat_fb, -use_fb)
    }
  }

  qc <- qc %>%
    dplyr::mutate(
      coord_valid = !is.na(lon) & !is.na(lat) & is.finite(lon) & is.finite(lat) & lon >= -180 & lon <= 180 & lat >= -90 & lat <= 90
    )

  if (file.exists(cfg$paths$service_area_path)) {
    sa <- tryCatch(sf::st_read(cfg$paths$service_area_path, quiet = TRUE) %>% sf::st_transform(4326), error = function(e) NULL)
    if (!is.null(sa) && nrow(sa) > 0) {
      pts_sf <- qc %>% dplyr::filter(coord_valid) %>% sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
      if (nrow(pts_sf) > 0) {
        within_idx <- lengths(sf::st_within(pts_sf, sa)) > 0
        qc <- qc %>% dplyr::left_join(tibble::tibble(zone_id = pts_sf$zone_id, in_service_area = within_idx), by = "zone_id")
      } else {
        qc$in_service_area <- NA
      }
    } else {
      qc$in_service_area <- NA
    }
  } else {
    qc$in_service_area <- NA
  }

  qc
}

build_point_df <- function(ids, qc_tbl, cfg) {
  ids_std <- standardize_zone_id(ids, cfg$geography$analysis_unit)
  qc_tbl %>%
    dplyr::filter(zone_id %in% ids_std) %>%
    dplyr::transmute(id = as.character(zone_id), lon = as.numeric(lon), lat = as.numeric(lat), coord_valid, in_service_area, point_source) %>%
    dplyr::filter(coord_valid) %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::select(id, lon, lat)
}

choose_probe_context <- function(cfg, failure_tbl, feed_name_arg = NULL, analysis_date_arg = NULL, time_window_id_arg = NULL) {
  routing_dates <- choose_routing_dates(cfg)
  win_tbl <- tibble::as_tibble(dplyr::bind_rows(cfg$routing$routing_windows))

  chosen <- list(feed_name = NULL, analysis_date = NULL, time_window_id = NULL)
  if (!is.null(feed_name_arg)) chosen$feed_name <- feed_name_arg
  if (!is.null(analysis_date_arg)) chosen$analysis_date <- as.character(as.Date(analysis_date_arg))
  if (!is.null(time_window_id_arg)) chosen$time_window_id <- time_window_id_arg

  if (nrow(failure_tbl) > 0) {
    ft <- failure_tbl
    if (!is.null(chosen$feed_name)) ft <- ft %>% dplyr::filter(feed_name == chosen$feed_name)
    if (!is.null(chosen$analysis_date)) ft <- ft %>% dplyr::filter(analysis_date == chosen$analysis_date)
    if (!is.null(chosen$time_window_id)) ft <- ft %>% dplyr::filter(time_window_id == chosen$time_window_id)
    if (nrow(ft) > 0) {
      pick <- ft %>% dplyr::arrange(dplyr::desc(n_failed_origins), analysis_date, time_window_id) %>% dplyr::slice(1)
      chosen$feed_name <- chosen$feed_name %||% as.character(pick$feed_name[[1]])
      chosen$analysis_date <- chosen$analysis_date %||% as.character(pick$analysis_date[[1]])
      chosen$time_window_id <- chosen$time_window_id %||% as.character(pick$time_window_id[[1]])
    }
  }

  if (is.null(chosen$feed_name) || is.null(chosen$analysis_date)) {
    if (nrow(routing_dates) == 0) stop("No routing dates available.", call. = FALSE)
    pick <- routing_dates %>% dplyr::arrange(analysis_date) %>% dplyr::slice(1)
    chosen$feed_name <- chosen$feed_name %||% as.character(pick$feed_name[[1]])
    chosen$analysis_date <- chosen$analysis_date %||% as.character(pick$analysis_date[[1]])
  }

  chosen$time_window_id <- chosen$time_window_id %||% as.character(win_tbl$time_window_id[[1]])
  list(
    feed_name = chosen$feed_name,
    analysis_date = as.Date(chosen$analysis_date),
    time_window_id = chosen$time_window_id,
    routing_dates = routing_dates,
    windows = win_tbl
  )
}

choose_test_pairs <- function(cfg, geog, qc_tbl, failure_tbl, context, pairs_csv_arg = NULL) {
  valid_ids <- qc_tbl %>% dplyr::filter(coord_valid) %>% dplyr::pull(zone_id) %>% unique() %>% as.character()
  add_pair <- function(tbl, origin, dest, label, source) {
    tibble::add_row(tbl,
      origin_id = as.character(standardize_zone_id(origin, cfg$geography$analysis_unit)),
      destination_id = as.character(standardize_zone_id(dest, cfg$geography$analysis_unit)),
      pair_label = as.character(label),
      pair_source = as.character(source)
    )
  }

  pairs <- tibble::tibble(origin_id = character(), destination_id = character(), pair_label = character(), pair_source = character())

  if (!is.null(pairs_csv_arg) && file.exists(pairs_csv_arg)) {
    user_pairs <- readr::read_csv(pairs_csv_arg, show_col_types = FALSE) %>%
      dplyr::transmute(
        origin_id = standardize_zone_id(.data[[1]], cfg$geography$analysis_unit),
        destination_id = standardize_zone_id(.data[[2]], cfg$geography$analysis_unit),
        pair_label = as.character(dplyr::coalesce(.data$pair_label, paste0(origin_id, "__", destination_id))),
        pair_source = "user_pairs_csv"
      )
    pairs <- dplyr::bind_rows(pairs, user_pairs)
  }

  # Known Boston probes, ignored if zones absent.
  known_pairs <- tibble::tribble(
    ~origin_id, ~destination_id, ~pair_label,
    "02108", "02139", "boston_core_to_cambridge",
    "02108", "02109", "downtown_adjacent",
    "02115", "02139", "fenway_to_cambridge",
    "02116", "02139", "backbay_to_cambridge",
    "02445", "02139", "brookline_to_cambridge",
    "01420", "01730", "outer_ring_control",
    "02108", "02108", "downtown_self",
    "01420", "01420", "outer_self"
  ) %>%
    dplyr::filter(origin_id %in% valid_ids, destination_id %in% valid_ids) %>%
    dplyr::mutate(pair_source = "known_pairs")
  pairs <- dplyr::bind_rows(pairs, known_pairs)

  top_pairs_path <- file.path(cfg$paths$od_dir, "top_od_pairs.csv")
  if (file.exists(top_pairs_path)) {
    tp <- read_csv_guess(top_pairs_path) %>%
      dplyr::mutate(
        origin_id = standardize_zone_id(origin_id, cfg$geography$analysis_unit),
        destination_id = standardize_zone_id(destination_id, cfg$geography$analysis_unit)
      ) %>%
      dplyr::filter(origin_id %in% valid_ids, destination_id %in% valid_ids, origin_id != destination_id)
    weight_col <- intersect(c("pair_weight", "weight", "trip_weight", "n_trips"), names(tp))
    if (length(weight_col) > 0) tp <- tp %>% dplyr::arrange(dplyr::desc(.data[[weight_col[[1]]]]))
    tp <- tp %>% dplyr::slice_head(n = 8) %>%
      dplyr::transmute(origin_id, destination_id, pair_label = paste0("top_od_", dplyr::row_number()), pair_source = "top_od_pairs")
    pairs <- dplyr::bind_rows(pairs, tp)
  }

  # Failed origins for the chosen context.
  failure_match <- tibble::tibble()
  if (nrow(failure_tbl) > 0 && all(c("feed_name", "analysis_date", "time_window_id", "failed_origins") %in% names(failure_tbl))) {
    failure_match <- failure_tbl %>%
      dplyr::filter(feed_name == context$feed_name, analysis_date == as.character(context$analysis_date), time_window_id == context$time_window_id)
  }
  failed_ids <- unique(unlist(strsplit(paste(failure_match$failed_origins %||% character(), collapse = ";"), ";", fixed = TRUE)))
  failed_ids <- failed_ids[nzchar(failed_ids)]
  failed_ids <- standardize_zone_id(failed_ids, cfg$geography$analysis_unit)
  failed_ids <- failed_ids[failed_ids %in% valid_ids]

  if (length(failed_ids) > 0) {
    pts <- qc_tbl %>% dplyr::filter(zone_id %in% valid_ids)
    for (oid in head(failed_ids, 8)) {
      o <- pts %>% dplyr::filter(zone_id == oid) %>% dplyr::slice(1)
      if (nrow(o) == 0) next
      near <- pts %>%
        dplyr::filter(zone_id != oid) %>%
        dplyr::mutate(d2 = (lon - o$lon[[1]])^2 + (lat - o$lat[[1]])^2) %>%
        dplyr::arrange(d2) %>%
        dplyr::slice(1)
      if (nrow(near) > 0) {
        pairs <- add_pair(pairs, oid, near$zone_id[[1]], paste0("failed_origin_nearest_", oid), "failed_origin_nearest")
      }
      pairs <- add_pair(pairs, oid, oid, paste0("failed_origin_self_", oid), "failed_origin_self")
    }
  }

  # Keep only valid ids and de-duplicate.
  pairs <- pairs %>%
    dplyr::mutate(
      origin_id = standardize_zone_id(origin_id, cfg$geography$analysis_unit),
      destination_id = standardize_zone_id(destination_id, cfg$geography$analysis_unit)
    ) %>%
    dplyr::filter(origin_id %in% valid_ids, destination_id %in% valid_ids) %>%
    dplyr::distinct(origin_id, destination_id, .keep_all = TRUE)

  if (is.null(pairs_csv_arg) || !file.exists(pairs_csv_arg)) {
    priority_levels <- c(
      "known_pairs",
      "failed_origin_nearest",
      "failed_origin_self",
      "top_od_pairs"
    )
    pairs <- pairs %>%
      dplyr::mutate(pair_source_priority = match(pair_source, priority_levels, nomatch = length(priority_levels) + 1L)) %>%
      dplyr::arrange(pair_source_priority, pair_label, origin_id, destination_id) %>%
      dplyr::slice_head(n = 6) %>%
      dplyr::select(-pair_source_priority)
  }

  pairs %>%
    dplyr::mutate(pair_id = paste0(origin_id, "__", destination_id))
}

run_ttm_direct <- function(network, origins_df, destinations_df, mode_vec, departure_datetime, time_window_minutes, cfg, n_threads = 1L) {
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
    n_threads = as.integer(n_threads),
    progress = FALSE
  )
}

normalize_raw_ttm <- function(x, cfg) {
  if (is.null(x)) return(tibble::tibble())
  x <- tibble::as_tibble(x)
  if (nrow(x) == 0) return(x)
  out <- x %>%
    dplyr::mutate(
      from_id = standardize_zone_id(as.character(from_id), cfg$geography$analysis_unit),
      to_id = standardize_zone_id(as.character(to_id), cfg$geography$analysis_unit)
    )
  tt_cols <- grep("^travel_time_p[0-9]+$", names(out), value = TRUE)
  if (length(tt_cols) > 0) out[tt_cols] <- lapply(out[tt_cols], function(col) suppressWarnings(as.numeric(col)))
  out
}

capture_r5r_sitrep <- function(out_path) {
  if (!"r5r_sitrep" %in% getNamespaceExports("r5r")) return(invisible(FALSE))
  txt <- tryCatch(capture.output(r5r::r5r_sitrep()), error = function(e) paste("r5r_sitrep failed:", conditionMessage(e)))
  save_text(txt, out_path)
  invisible(TRUE)
}

run_find_snap <- function(network, pts_df, label, out_path) {
  if (!"find_snap" %in% getNamespaceExports("r5r")) return(invisible(NULL))
  if (nrow(pts_df) == 0) return(invisible(NULL))
  fn <- get("find_snap", asNamespace("r5r"))
  fml <- names(formals(fn))
  args <- list()
  if ("r5r_network" %in% fml) args$r5r_network <- network
  if ("network" %in% fml && is.null(args$network)) args$network <- network
  if ("points" %in% fml) args$points <- pts_df
  if ("mode" %in% fml) args$mode <- c("WALK")
  if ("max_distance" %in% fml) args$max_distance <- 1600
  out <- tryCatch(do.call(fn, args), error = function(e) tibble::tibble(probe_set = label, snap_error = conditionMessage(e)))
  out <- tibble::as_tibble(out)
  if (!"probe_set" %in% names(out)) out$probe_set <- label
  readr::write_csv(out, out_path)
  invisible(out)
}

safe_run <- function(expr) {
  tryCatch(list(status = "ok", result = force(expr), error_message = NA_character_),
           error = function(e) list(status = "error", result = NULL, error_message = conditionMessage(e)))
}

service_levels_for_context <- function(cfg, feed_name, analysis_date) {
  feed_registry <- make_feed_registry_for_routing(cfg)
  gtfs_files <- feed_registry %>% dplyr::filter(feed_name == !!feed_name) %>% dplyr::pull(gtfs_files)
  if (length(gtfs_files) == 0) return(tibble::tibble())
  gtfs_path <- gtfs_files[[1]][[1]] %||% gtfs_files[[1]]
  svc <- tryCatch(parse_gtfs_service_levels(gtfs_path), error = function(e) tibble::tibble())
  if (nrow(svc) == 0) return(tibble::tibble())
  svc %>%
    dplyr::mutate(feed_name = feed_name) %>%
    dplyr::group_by(feed_name) %>%
    dplyr::mutate(max_services = max(n_services, na.rm = TRUE), service_ratio = dplyr::if_else(max_services > 0, n_services / max_services, NA_real_)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(date == as.Date(analysis_date))
}


inspect_gtfs_route_inventory <- function(gtfs_zip_path) {
  td <- tempfile("gtfs_inv_")
  fs::dir_create(td)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(gtfs_zip_path, exdir = td)
  routes_path <- file.path(td, "routes.txt")
  trips_path <- file.path(td, "trips.txt")
  stop_times_path <- file.path(td, "stop_times.txt")
  if (!file.exists(routes_path)) return(tibble::tibble())
  routes <- readr::read_csv(routes_path, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
  trips <- if (file.exists(trips_path)) readr::read_csv(trips_path, show_col_types = FALSE, col_types = readr::cols(.default = "c")) else tibble::tibble()
  stop_times <- if (file.exists(stop_times_path)) readr::read_csv(stop_times_path, show_col_types = FALSE, col_types = readr::cols(.default = "c")) else tibble::tibble()
  if (!"route_type" %in% names(routes)) routes$route_type <- NA_character_
  routes <- routes %>% dplyr::mutate(route_type_num = suppressWarnings(as.integer(route_type)))
  trip_counts <- if (nrow(trips) > 0 && "route_id" %in% names(trips)) trips %>% dplyr::count(route_id, name = "n_trips") else tibble::tibble(route_id = character(), n_trips = integer())
  st_counts <- if (nrow(stop_times) > 0 && "trip_id" %in% names(stop_times) && nrow(trips) > 0 && "trip_id" %in% names(trips)) {
    stop_times %>% dplyr::count(trip_id, name = "n_stop_times") %>% dplyr::right_join(trips %>% dplyr::select(trip_id, route_id), by = "trip_id") %>% dplyr::group_by(route_id) %>% dplyr::summarise(n_stop_times = sum(n_stop_times, na.rm = TRUE), .groups = "drop")
  } else tibble::tibble(route_id = character(), n_stop_times = integer())
  routes %>%
    dplyr::left_join(trip_counts, by = "route_id") %>%
    dplyr::left_join(st_counts, by = "route_id") %>%
    dplyr::mutate(n_trips = dplyr::coalesce(n_trips, 0L), n_stop_times = dplyr::coalesce(n_stop_times, 0L)) %>%
    dplyr::group_by(route_type_num) %>%
    dplyr::summarise(n_routes = dplyr::n(), n_trips = sum(n_trips, na.rm = TRUE), n_stop_times = sum(n_stop_times, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(route_type_num)
}

run_same_network_mode_variants <- function(cfg, context, base_gtfs_path, out_dir, network, origins_unique, destinations_unique, requested_pairs, departure_datetime, time_window_minutes) {
  inventory <- inspect_gtfs_route_inventory(base_gtfs_path)
  readr::write_csv(inventory, file.path(out_dir, "gtfs_route_type_inventory_original.csv"))

  mode_specs <- tibble::tibble(
    variant_label = c("walk_only", "bus_only", "subway_only", "rail_only", "bus_plus_subway", "configured_modes", "all_transit"),
    mode_vec = list(
      c("WALK"),
      c("WALK", "BUS"),
      c("WALK", "SUBWAY"),
      c("WALK", "RAIL"),
      c("WALK", "BUS", "SUBWAY"),
      as.character(unlist(cfg$routing$modes)),
      c("WALK", "TRANSIT")
    )
  )

  service_tbl <- service_levels_for_context(cfg, context$feed_name, context$analysis_date)
  if (nrow(service_tbl) > 0) {
    mode_service_levels <- mode_specs %>% dplyr::select(variant_label) %>% dplyr::cross_join(service_tbl)
  } else {
    mode_service_levels <- tibble::tibble()
  }

  mode_matrix_summary <- tibble::tibble()
  mode_matrix_raw <- tibble::tibble()
  mode_pair_hits <- tibble::tibble()

  for (i in seq_len(nrow(mode_specs))) {
    row <- mode_specs[i, ]
    mode_vec <- row$mode_vec[[1]]
    probe <- safe_run(run_ttm_direct(network, origins_unique, destinations_unique, mode_vec, departure_datetime, time_window_minutes, cfg, n_threads = 1L))
    raw <- if (identical(probe$status, "ok")) normalize_raw_ttm(probe$result, cfg) else tibble::tibble()
    if (nrow(raw) > 0) raw <- raw %>% dplyr::mutate(variant_label = row$variant_label[[1]])
    mode_matrix_raw <- dplyr::bind_rows(mode_matrix_raw, raw)

    mode_matrix_summary <- dplyr::bind_rows(mode_matrix_summary, tibble::tibble(
      variant_label = row$variant_label[[1]],
      mode_string = paste(mode_vec, collapse = ";"),
      status = probe$status,
      error_message = probe$error_message,
      n_rows_returned = nrow(raw),
      n_exact_requested_pairs_hit = if (nrow(raw) > 0) sum(paste0(raw$from_id, "__", raw$to_id) %in% requested_pairs$pair_id) else 0L,
      n_self_pairs_returned = if (nrow(raw) > 0) sum(raw$from_id == raw$to_id, na.rm = TRUE) else 0L,
      n_unique_from = if (nrow(raw) > 0) dplyr::n_distinct(raw$from_id) else 0L,
      n_unique_to = if (nrow(raw) > 0) dplyr::n_distinct(raw$to_id) else 0L
    ))

    if (nrow(raw) > 0) {
      hits <- requested_pairs %>%
        dplyr::left_join(raw %>% dplyr::mutate(pair_id = paste0(from_id, "__", to_id)) %>% dplyr::group_by(pair_id) %>% dplyr::summarise(raw_rows_for_pair = dplyr::n(), .groups = "drop"), by = "pair_id") %>%
        dplyr::mutate(variant_label = row$variant_label[[1]], exact_pair_hit = !is.na(raw_rows_for_pair) & raw_rows_for_pair > 0L)
    } else {
      hits <- requested_pairs %>% dplyr::mutate(variant_label = row$variant_label[[1]], raw_rows_for_pair = 0L, exact_pair_hit = FALSE)
    }
    mode_pair_hits <- dplyr::bind_rows(mode_pair_hits, hits)
  }

  readr::write_csv(mode_service_levels, file.path(out_dir, "gtfs_variant_service_levels.csv"))
  readr::write_csv(mode_matrix_summary, file.path(out_dir, "variant_matrix_probe_summary.csv"))
  readr::write_csv(mode_matrix_raw, file.path(out_dir, "variant_matrix_probe_raw.csv"))
  readr::write_csv(mode_pair_hits, file.path(out_dir, "variant_matrix_requested_pair_hits.csv"))

  list(
    inventory = inventory,
    build_summary = mode_specs %>% dplyr::mutate(status = "same_network_mode_probe", mode_string = purrr::map_chr(mode_vec, ~ paste(.x, collapse = ";"))) %>% dplyr::select(variant_label, status, mode_string),
    matrix_summary = mode_matrix_summary,
    pair_hits = mode_pair_hits,
    service_levels = mode_service_levels
  )
}

# Snapshot context and prior failures.
routing_dates_all <- choose_routing_dates(cfg)
readr::write_csv(routing_dates_all, file.path(out_dir, "routing_dates_selected.csv"))
failure_tbl <- extract_failure_contexts(cfg$paths$logs_dir)
readr::write_csv(failure_tbl, file.path(out_dir, "prior_failure_contexts.csv"))
context <- choose_probe_context(cfg, failure_tbl, feed_name_arg, analysis_date_arg, time_window_id_arg)
write_json_pretty(list(
  config_path = config_path,
  source_id = cfg$active_survey_source_id,
  analysis_unit = cfg$geography$analysis_unit,
  run_id = cfg$run$run_id,
  feed_name = context$feed_name,
  analysis_date = as.character(context$analysis_date),
  time_window_id = context$time_window_id,
  java_memory = java_boot$java_memory,
  java_active_processors = java_boot$java_active_processors
), file.path(out_dir, "probe_context.json"))

capture_r5r_sitrep(file.path(out_dir, "r5r_sitrep.txt"))

geog <- get_active_geography_for_routing(cfg)
qc_tbl <- build_zone_point_qc(geog, cfg)
readr::write_csv(qc_tbl, file.path(out_dir, "zone_point_qc_probe.csv"))

pair_manifest <- choose_test_pairs(cfg, geog, qc_tbl, failure_tbl, context, pairs_csv_arg)
if (nrow(pair_manifest) == 0) stop("No valid test pairs could be built. Provide a pairs CSV or fix routing point QC first.", call. = FALSE)
readr::write_csv(pair_manifest, file.path(out_dir, "test_pair_manifest.csv"))

point_manifest <- dplyr::bind_rows(
  pair_manifest %>% dplyr::transmute(zone_id = origin_id, role = "origin"),
  pair_manifest %>% dplyr::transmute(zone_id = destination_id, role = "destination")
) %>%
  dplyr::distinct() %>%
  dplyr::left_join(qc_tbl, by = "zone_id")
readr::write_csv(point_manifest, file.path(out_dir, "test_point_manifest.csv"))

service_tbl <- service_levels_for_context(cfg, context$feed_name, context$analysis_date)
readr::write_csv(service_tbl, file.path(out_dir, "service_levels_for_probe_date.csv"))

# Build network.
feed_registry <- make_feed_registry_for_routing(cfg)
gtfs_files <- feed_registry %>% dplyr::filter(feed_name == context$feed_name) %>% dplyr::pull(gtfs_files)
if (length(gtfs_files) == 0) stop(paste0("Could not resolve gtfs_files for feed ", context$feed_name), call. = FALSE)
gtfs_files <- gtfs_files[[1]]
network <- NULL
on.exit({ if (!is.null(network) && inherits(network, "r5r_network")) try(r5r::stop_r5(network), silent = TRUE) }, add = TRUE)
network <- build_r5_network_object(cfg, context$feed_name, gtfs_files)

win_row <- context$windows %>% dplyr::filter(time_window_id == context$time_window_id) %>% dplyr::slice(1)
if (nrow(win_row) == 0) stop(paste0("Could not find routing window ", context$time_window_id), call. = FALSE)
departure_datetime <- combine_date_time(context$analysis_date, win_row$start_time[[1]], cfg$project$timezone)
configured_time_window_minutes <- window_minutes(win_row$start_time[[1]], win_row$end_time[[1]])
time_window_minutes <- as.integer(min(1L, configured_time_window_minutes))
write_json_pretty(list(
  departure_datetime = as.character(departure_datetime),
  configured_time_window_minutes = as.integer(configured_time_window_minutes),
  probe_time_window_minutes = as.integer(time_window_minutes),
  routing_modes = unlist(cfg$routing$modes),
  percentiles = unlist(cfg$routing$percentiles),
  max_walk_time = cfg$routing$max_walk_time,
  max_trip_duration = cfg$routing$max_trip_duration,
  max_rides = cfg$routing$max_rides,
  walk_speed = cfg$routing$walk_speed,
  configured_n_threads = cfg$routing$n_threads
), file.path(out_dir, "routing_runtime_snapshot.json"))

# Prepare point tables.
origins_unique <- build_point_df(unique(pair_manifest$origin_id), qc_tbl, cfg)
destinations_unique <- build_point_df(unique(pair_manifest$destination_id), qc_tbl, cfg)
readr::write_csv(origins_unique, file.path(out_dir, "probe_origins_points.csv"))
readr::write_csv(destinations_unique, file.path(out_dir, "probe_destinations_points.csv"))
run_find_snap(network, origins_unique, "origins", file.path(out_dir, "probe_origins_find_snap.csv"))
run_find_snap(network, destinations_unique, "destinations", file.path(out_dir, "probe_destinations_find_snap.csv"))

# Single-pair probes.
run_single_probe <- function(pair_row, runner_label, mode_label, mode_vec, n_threads = 1L, use_wrapper = FALSE) {
  o <- origins_unique %>% dplyr::filter(id == pair_row$origin_id) %>% dplyr::slice(1)
  d <- destinations_unique %>% dplyr::filter(id == pair_row$destination_id) %>% dplyr::slice(1)
  if (nrow(o) == 0 || nrow(d) == 0) {
    return(list(
      summary = tibble::tibble(pair_id = pair_row$pair_id, pair_label = pair_row$pair_label, runner = runner_label, mode_label = mode_label,
                               status = "skipped", error_message = "missing valid origin/destination point", n_rows_returned = NA_integer_),
      raw = tibble::tibble()
    ))
  }

  if (use_wrapper) {
    routing_cfg <- cfg$routing
    routing_cfg$n_threads <- as.integer(n_threads)
    probe <- safe_run(compute_ttm_one_chunk(network, o, d, departure_datetime, routing_cfg, time_window_minutes, progress = FALSE))
  } else {
    probe <- safe_run(run_ttm_direct(network, o, d, mode_vec, departure_datetime, time_window_minutes, cfg, n_threads = n_threads))
  }

  raw <- if (identical(probe$status, "ok")) normalize_raw_ttm(probe$result, cfg) else tibble::tibble()
  if (nrow(raw) > 0) raw <- raw %>% dplyr::mutate(pair_id = pair_row$pair_id, pair_label = pair_row$pair_label, runner = runner_label, mode_label = mode_label)

  summary <- tibble::tibble(
    pair_id = pair_row$pair_id,
    pair_label = pair_row$pair_label,
    origin_id = pair_row$origin_id,
    destination_id = pair_row$destination_id,
    runner = runner_label,
    mode_label = mode_label,
    status = probe$status,
    error_message = probe$error_message,
    n_rows_returned = nrow(raw),
    returned_exact_pair = any(raw$from_id == pair_row$origin_id & raw$to_id == pair_row$destination_id),
    returned_self_pair = any(raw$from_id == pair_row$origin_id & raw$to_id == pair_row$origin_id),
    returned_other_pairs = any(!(raw$from_id == pair_row$origin_id & raw$to_id == pair_row$destination_id) & !(raw$from_id == pair_row$origin_id & raw$to_id == pair_row$origin_id))
  )

  list(summary = summary, raw = raw)
}

single_results <- purrr::map(seq_len(nrow(pair_manifest)), function(i) {
  row <- pair_manifest[i, ]
  list(
    direct_walk = run_single_probe(row, "direct", "walk_only", c("WALK"), n_threads = 1L, use_wrapper = FALSE),
    direct_transit = run_single_probe(row, "direct", "configured_modes", unlist(cfg$routing$modes), n_threads = 1L, use_wrapper = FALSE),
    wrapper_transit = run_single_probe(row, "project_wrapper", "configured_modes", unlist(cfg$routing$modes), n_threads = 1L, use_wrapper = TRUE)
  )
})

single_summary <- purrr::map_dfr(single_results, ~ dplyr::bind_rows(.x$direct_walk$summary, .x$direct_transit$summary, .x$wrapper_transit$summary))
single_raw <- purrr::map_dfr(single_results, ~ dplyr::bind_rows(.x$direct_walk$raw, .x$direct_transit$raw, .x$wrapper_transit$raw))
readr::write_csv(single_summary, file.path(out_dir, "single_pair_summary.csv"))
readr::write_csv(single_raw, file.path(out_dir, "single_pair_raw.csv"))

# Small matrix probes.
requested_pairs <- pair_manifest %>% dplyr::select(pair_id, pair_label, origin_id, destination_id)
write_json_pretty(list(n_origins = nrow(origins_unique), n_destinations = nrow(destinations_unique), n_requested_pairs = nrow(requested_pairs)), file.path(out_dir, "matrix_probe_sizes.json"))

matrix_runs <- list(
  direct_walk = list(runner = "direct", mode_label = "walk_only", mode_vec = c("WALK"), n_threads = 1L, use_wrapper = FALSE),
  direct_transit = list(runner = "direct", mode_label = "configured_modes", mode_vec = unlist(cfg$routing$modes), n_threads = 1L, use_wrapper = FALSE),
  wrapper_transit = list(runner = "project_wrapper", mode_label = "configured_modes", mode_vec = unlist(cfg$routing$modes), n_threads = 1L, use_wrapper = TRUE)
)

matrix_summary <- tibble::tibble()
matrix_raw_all <- tibble::tibble()
for (nm in names(matrix_runs)) {
  spec <- matrix_runs[[nm]]
  probe <- if (spec$use_wrapper) {
    routing_cfg <- cfg$routing
    routing_cfg$n_threads <- as.integer(spec$n_threads)
    safe_run(compute_ttm_one_chunk(network, origins_unique, destinations_unique, departure_datetime, routing_cfg, time_window_minutes, progress = FALSE))
  } else {
    safe_run(run_ttm_direct(network, origins_unique, destinations_unique, spec$mode_vec, departure_datetime, time_window_minutes, cfg, n_threads = spec$n_threads))
  }
  raw <- if (identical(probe$status, "ok")) normalize_raw_ttm(probe$result, cfg) else tibble::tibble()
  if (nrow(raw) > 0) raw <- raw %>% dplyr::mutate(runner = spec$runner, mode_label = spec$mode_label)
  matrix_raw_all <- dplyr::bind_rows(matrix_raw_all, raw)
  matrix_summary <- dplyr::bind_rows(matrix_summary, tibble::tibble(
    runner = spec$runner,
    mode_label = spec$mode_label,
    status = probe$status,
    error_message = probe$error_message,
    n_rows_returned = nrow(raw),
    n_exact_requested_pairs_hit = if (nrow(raw) > 0) sum(paste0(raw$from_id, "__", raw$to_id) %in% requested_pairs$pair_id) else 0L,
    n_self_pairs_returned = if (nrow(raw) > 0) sum(raw$from_id == raw$to_id, na.rm = TRUE) else 0L,
    n_unique_from = if (nrow(raw) > 0) dplyr::n_distinct(raw$from_id) else 0L,
    n_unique_to = if (nrow(raw) > 0) dplyr::n_distinct(raw$to_id) else 0L
  ))
}
readr::write_csv(matrix_summary, file.path(out_dir, "matrix_probe_summary.csv"))
readr::write_csv(matrix_raw_all, file.path(out_dir, "matrix_probe_raw.csv"))

pair_hits <- if (nrow(matrix_raw_all) > 0) {
  requested_pairs %>%
    dplyr::cross_join(matrix_summary %>% dplyr::select(runner, mode_label)) %>%
    dplyr::left_join(
      matrix_raw_all %>%
        dplyr::mutate(pair_id = paste0(from_id, "__", to_id)) %>%
        dplyr::group_by(runner, mode_label, pair_id) %>%
        dplyr::summarise(raw_rows_for_pair = dplyr::n(), .groups = "drop"),
      by = c("runner", "mode_label", "pair_id")
    ) %>%
    dplyr::mutate(exact_pair_hit = !is.na(raw_rows_for_pair) & raw_rows_for_pair > 0L) %>%
    dplyr::left_join(
      matrix_raw_all %>%
        dplyr::group_by(runner, mode_label, from_id, to_id) %>%
        dplyr::summarise(.groups = "drop"),
      by = c("runner", "mode_label", "origin_id" = "from_id", "destination_id" = "to_id")
    )
} else {
  requested_pairs %>%
    dplyr::cross_join(matrix_summary %>% dplyr::select(runner, mode_label)) %>%
    dplyr::mutate(raw_rows_for_pair = 0L, exact_pair_hit = FALSE)
}
readr::write_csv(pair_hits, file.path(out_dir, "matrix_requested_pair_hits.csv"))

# Pair-level comparison between single probes and matrix probes.
pair_rollup <- requested_pairs %>%
  dplyr::left_join(
    single_summary %>%
      dplyr::group_by(pair_id, runner, mode_label) %>%
      dplyr::summarise(
        single_status = paste(unique(status), collapse = ";"),
        single_error = paste(unique(na.omit(error_message)), collapse = " | "),
        single_returned_exact_pair = any(returned_exact_pair, na.rm = TRUE),
        single_returned_self_pair = any(returned_self_pair, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "pair_id"
  ) %>%
  dplyr::left_join(pair_hits %>% dplyr::select(pair_id, runner, mode_label, exact_pair_hit), by = c("pair_id", "runner", "mode_label"))
readr::write_csv(pair_rollup, file.path(out_dir, "pair_level_rollup.csv"))

base_gtfs_path <- gtfs_files[[1]][[1]] %||% gtfs_files[[1]]
variant_outputs <- run_same_network_mode_variants(
  cfg = cfg,
  context = context,
  base_gtfs_path = base_gtfs_path,
  out_dir = out_dir,
  network = network,
  origins_unique = origins_unique,
  destinations_unique = destinations_unique,
  requested_pairs = requested_pairs,
  departure_datetime = departure_datetime,
  time_window_minutes = time_window_minutes
)

summary_payload <- list(
  out_dir = out_dir,
  feed_name = context$feed_name,
  analysis_date = as.character(context$analysis_date),
  time_window_id = context$time_window_id,
  n_pairs = nrow(pair_manifest),
  n_origins = nrow(origins_unique),
  n_destinations = nrow(destinations_unique),
  n_invalid_points_in_manifest = sum(!point_manifest$coord_valid, na.rm = TRUE),
  matrix_summary = matrix_summary,
  variant_matrix_summary = variant_outputs$matrix_summary,
  variant_build_summary = variant_outputs$build_summary,
  single_summary_status_counts = single_summary %>% dplyr::count(runner, mode_label, status),
  files = list.files(out_dir)
)
write_json_pretty(summary_payload, file.path(out_dir, "probe_summary.json"))

safe_message("Wrote minimal R5 diagnostics to: ", out_dir)
print(matrix_summary)
