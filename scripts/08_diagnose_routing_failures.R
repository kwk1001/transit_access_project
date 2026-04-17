get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE))
  }
  for (i in rev(seq_along(sys.frames()))) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile)) {
      return(normalizePath(ofile, winslash = "/", mustWork = FALSE))
    }
  }
  NULL
}

bootstrap_config_args <- function(default_config, project_root) {
  args <- commandArgs(trailingOnly = TRUE)
  config_path <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else default_config
  if (!grepl("^(~|/|[A-Za-z]:)", config_path)) {
    config_path <- file.path(project_root, config_path)
  }
  source_id <- if (length(args) >= 2 && nzchar(args[[2]])) args[[2]] else NULL
  analysis_unit <- if (length(args) >= 3 && nzchar(args[[3]])) tolower(args[[3]]) else NULL
  list(
    config_path = normalizePath(config_path, winslash = "/", mustWork = FALSE),
    source_id = source_id,
    analysis_unit = analysis_unit
  )
}

extract_failed_origins_from_logs <- function(log_dir) {
  files <- if (dir.exists(log_dir)) fs::dir_ls(log_dir, glob = "routing_origin_fallback_*.json") else character()
  if (length(files) == 0) return(character())

  out <- c()
  for (f in files) {
    j <- tryCatch(jsonlite::read_json(f, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(j) || is.null(j$failed_origins)) next

    fo <- j$failed_origins
    if (is.data.frame(fo) && "origin_id" %in% names(fo)) {
      out <- c(out, as.character(fo$origin_id))
    } else if (is.list(fo)) {
      ids <- purrr::map_chr(fo, ~ as.character(.x$origin_id %||% NA_character_))
      out <- c(out, ids)
    }
  }

  unique(out[!is.na(out) & nzchar(out)])
}

probe_origin <- function(origin_id, origins_all, destinations_all, network, cfg, probe_datetime, probe_window_minutes) {
  origin_row <- origins_all %>% dplyr::filter(id == origin_id) %>% dplyr::slice(1)
  if (nrow(origin_row) == 0) {
    return(tibble::tibble(
      origin_id = origin_id,
      probe_type = "point_lookup",
      status = "fail",
      error_message = "origin not found in routing points",
      n_destinations = 0
    ))
  }

  cand_dest <- destinations_all %>%
    dplyr::filter(id != origin_id) %>%
    dplyr::mutate(d2 = (lon - origin_row$lon[[1]])^2 + (lat - origin_row$lat[[1]])^2) %>%
    dplyr::arrange(d2)

  one_dest <- cand_dest %>% dplyr::slice(1) %>% dplyr::select(id, lon, lat)
  ten_dest <- cand_dest %>% dplyr::slice_head(n = 10) %>% dplyr::select(id, lon, lat)

  run_probe <- function(dest_df, label) {
    res <- tryCatch(
      {
        out <- r5r::travel_time_matrix(
          r5r_network = network,
          origins = origin_row,
          destinations = dest_df,
          mode = unlist(cfg$routing$modes),
          departure_datetime = probe_datetime,
          time_window = as.integer(probe_window_minutes),
          percentiles = unlist(cfg$routing$percentiles),
          max_walk_time = cfg$routing$max_walk_time,
          max_trip_duration = cfg$routing$max_trip_duration,
          walk_speed = cfg$routing$walk_speed,
          max_rides = cfg$routing$max_rides,
          n_threads = 1,
          progress = FALSE
        )
        tibble::tibble(
          origin_id = origin_id,
          probe_type = label,
          status = "ok",
          error_message = NA_character_,
          n_destinations = nrow(dest_df),
          n_rows_returned = nrow(out)
        )
      },
      error = function(e) {
        tibble::tibble(
          origin_id = origin_id,
          probe_type = label,
          status = "fail",
          error_message = tryCatch(conditionMessage(e), error = function(...) paste0(e)),
          n_destinations = nrow(dest_df),
          n_rows_returned = NA_integer_
        )
      }
    )
    res
  }

  dplyr::bind_rows(
    run_probe(one_dest, "single_origin_single_destination"),
    run_probe(ten_dest, "single_origin_ten_destinations")
  )
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

script_path <- get_script_path()
project_root <- if (!is.null(script_path)) {
  normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
} else {
  getwd()
}
source(file.path(project_root, "R", "packages.R"))
boot <- bootstrap_config_args(default_config = file.path("config", "boston.yml"), project_root = project_root)
java_boot <- peek_java_config(boot$config_path)
configure_java_for_r5r(java_memory = java_boot$java_memory, java_active_processors = java_boot$java_active_processors)
source(file.path(project_root, "R", "load_project.R"))
load_project(project_root)

cfg <- load_project_config(boot$config_path, boot$source_id)
if (!is.null(boot$analysis_unit)) {
  cfg <- apply_runtime_overrides(cfg, list(analysis_unit = boot$analysis_unit))
}
ensure_project_dirs(cfg)

failed_origins <- extract_failed_origins_from_logs(cfg$paths$logs_dir)
if (length(failed_origins) == 0) {
  stop("No failed origins found in routing_origin_fallback_*.json under logs_dir.", call. = FALSE)
}

geog <- get_active_geography_for_routing(cfg)
routing_centroids <- geog$routing_zone_centroids %||% geog$analysis_zone_centroids

origins_all <- make_routing_points(routing_centroids, failed_origins, cfg)
destinations_all <- make_routing_points(routing_centroids, geog$analysis_zones$zone_id, cfg)

qc_path <- file.path(cfg$paths$logs_dir, "zone_point_qc.csv")
qc_tbl <- if (file.exists(qc_path)) read_csv_guess(qc_path) else tibble::tibble(zone_id = character())

routing_dates <- choose_routing_dates(cfg)
if (nrow(routing_dates) == 0) stop("No routing dates available to probe.", call. = FALSE)
probe_date <- routing_dates %>% dplyr::arrange(analysis_date) %>% dplyr::slice(1)

win_tbl <- tibble::as_tibble(dplyr::bind_rows(cfg$routing$routing_windows))
if (nrow(win_tbl) == 0) stop("No routing windows configured.", call. = FALSE)
probe_win <- win_tbl %>% dplyr::slice(1)

probe_datetime <- combine_date_time(probe_date$analysis_date, probe_win$start_time, cfg$project$timezone)
probe_window_minutes <- window_minutes(probe_win$start_time, probe_win$end_time)

feed_name <- as.character(probe_date$feed_name[[1]])
feed_registry <- make_feed_registry_for_routing(cfg)
gtfs_files <- feed_registry %>% dplyr::filter(feed_name == !!feed_name) %>% dplyr::pull(gtfs_files) %>% .[[1]]

network <- NULL
on.exit({
  if (!is.null(network) && inherits(network, "r5r_network")) {
    try(r5r::stop_r5(network), silent = TRUE)
  }
}, add = TRUE)

network <- build_r5_network_object(cfg, feed_name, gtfs_files)

probe_results <- purrr::map_dfr(failed_origins, probe_origin,
  origins_all = origins_all,
  destinations_all = destinations_all,
  network = network,
  cfg = cfg,
  probe_datetime = probe_datetime,
  probe_window_minutes = probe_window_minutes
)

qc_join <- probe_results %>%
  left_join(qc_tbl %>% dplyr::rename(origin_id = zone_id), by = "origin_id") %>%
  dplyr::mutate(
    probe_feed = feed_name,
    probe_date = as.character(probe_date$analysis_date[[1]]),
    probe_time_window_id = as.character(probe_win$time_window_id[[1]])
  )

out_path <- file.path(cfg$paths$logs_dir, "routing_origin_probe_results.csv")
readr::write_csv(qc_join, out_path)
message("Wrote diagnostics: ", out_path)

summary_tbl <- qc_join %>%
  dplyr::group_by(probe_type, status) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")
print(summary_tbl)
