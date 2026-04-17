make_feed_registry_for_routing <- function(cfg) {
  tibble(
    feed_name = purrr::map_chr(cfg$gtfs_feeds, "feed_name"),
    valid_start = as.Date(purrr::map(cfg$gtfs_feeds, "valid_start") %>% unlist()),
    valid_end_exclusive = as.Date(purrr::map(cfg$gtfs_feeds, "valid_end_exclusive") %>% unlist()),
    gtfs_files = purrr::map(cfg$gtfs_feeds, "gtfs_files")
  )
}

prepare_network_input_dir <- function(cfg, feed_name, gtfs_files) {
  target_dir <- file.path(cfg$paths$network_dir, feed_name, "input")
  fs::dir_create(target_dir)

  osm_target <- file.path(target_dir, basename(cfg$osm$local_pbf_path))
  if (!file_is_nonempty(osm_target, min_bytes = 1024 * 1024)) {
    file.copy(cfg$osm$local_pbf_path, osm_target, overwrite = TRUE)
  }

  purrr::walk(gtfs_files, function(gtfs_path) {
    validate_gtfs_zip_quick(gtfs_path)
    gtfs_target <- file.path(target_dir, basename(gtfs_path))
    if (!file_is_nonempty(gtfs_target, min_bytes = 1024)) {
      file.copy(gtfs_path, gtfs_target, overwrite = TRUE)
    }
  })

  target_dir
}

build_r5_network_object <- function(cfg, feed_name, gtfs_files) {
  input_dir <- prepare_network_input_dir(cfg, feed_name, gtfs_files)
  network_dat <- file.path(input_dir, "network.dat")

  if (file_is_nonempty(network_dat, min_bytes = 1024) && !isTRUE(cfg$routing$overwrite_networks)) {
    message("Loading cached R5 network for ", feed_name)
  } else {
    message("Building R5 network for ", feed_name)
  }

  build_once <- function(overwrite_flag = FALSE) {
    r5r::build_network(
      data_path = input_dir,
      verbose = FALSE,
      overwrite = overwrite_flag
    )
  }

  out <- tryCatch(
    build_once(isTRUE(cfg$routing$overwrite_networks)),
    error = function(e) {
      if (file.exists(network_dat) && !isTRUE(cfg$routing$overwrite_networks)) {
        message("Cached network for ", feed_name, " could not be loaded. Deleting network.dat and rebuilding once.")
        unlink(network_dat)
        return(build_once(TRUE))
      }
      stop(e)
    }
  )

  if (is.null(out) || !inherits(out, "r5r_network")) {
    stop(
      paste0(
        "Failed to build/load R5 network for ", feed_name,
        ". This often means Java heap memory was too small or the previous JVM session started without the intended java.parameters. ",
        "Restart R, make sure java.parameters is set before r5r loads, and try again."
      ),
      call. = FALSE
    )
  }

  out
}

collect_gtfs_stops_for_service_area <- function(cfg) {
  stop_sf_list <- purrr::map(cfg$gtfs_feeds, function(feed) {
    purrr::map_dfr(feed$gtfs_files, function(gtfs_file) {
      validate_gtfs_zip_quick(gtfs_file)
      gt <- tidytransit::read_gtfs(gtfs_file)
      gt$stops %>%
        filter(!is.na(stop_lon), !is.na(stop_lat)) %>%
        sf::st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE)
    })
  })

  dplyr::bind_rows(stop_sf_list)
}

build_service_area <- function(cfg, geography_outputs) {
  service_area_path <- file.path(cfg$paths$service_area_dir, "service_area.gpkg")
  served_tracts_path <- file.path(cfg$paths$service_area_dir, "tracts_served.gpkg")
  served_centroids_path <- file.path(cfg$paths$service_area_dir, "tract_centroids_served.gpkg")

  if (!isTRUE(cfg$run_options$force_downloads) && file_is_nonempty(service_area_path, 100) && (!isTRUE(cfg$geography$restrict_to_gtfs_service_area) || all_files_nonempty(c(served_tracts_path, served_centroids_path), 100))) {
    message("Using cached GTFS service area outputs.")
    return(invisible(sf::st_read(service_area_path, quiet = TRUE)))
  }

  stops_sf <- collect_gtfs_stops_for_service_area(cfg)
  if (nrow(stops_sf) == 0) {
    return(NULL)
  }

  service_area <- stops_sf %>%
    sf::st_transform(3857) %>%
    sf::st_buffer(cfg$geography$service_buffer_m) %>%
    sf::st_union() %>%
    sf::st_make_valid() %>%
    sf::st_set_precision(1) %>%
    sf::st_make_valid()

  geom_types <- unique(as.character(sf::st_geometry_type(service_area)))
  needs_extract <- any(geom_types %in% c("GEOMETRYCOLLECTION", "MULTISURFACE", "GEOMETRY"))
  if (needs_extract) {
    service_area <- sf::st_collection_extract(service_area, "POLYGON")
  }

  service_area <- service_area %>%
    sf::st_union() %>%
    sf::st_make_valid() %>%
    sf::st_transform(4326)

  service_area_sf <- sf::st_as_sf(tibble::tibble(service_area_id = 1, geometry = service_area))
  sf::st_write(service_area_sf, service_area_path, delete_dsn = TRUE, quiet = TRUE)

  if (isTRUE(cfg$geography$restrict_to_gtfs_service_area)) {
    old_s2 <- sf::sf_use_s2()
    on.exit(sf::sf_use_s2(old_s2), add = TRUE)
    sf::sf_use_s2(FALSE)

    tracts_3857 <- sf::st_transform(geography_outputs$tracts, 3857)
    service_3857 <- sf::st_transform(service_area_sf, 3857)
    tract_hits <- lengths(sf::st_intersects(tracts_3857, service_3857)) > 0
    served_tracts <- geography_outputs$tracts[tract_hits, , drop = FALSE]

    centroids_3857 <- sf::st_transform(geography_outputs$tract_centroids, 3857)
    centroid_hits <- lengths(sf::st_intersects(centroids_3857, service_3857)) > 0
    served_centroids <- geography_outputs$tract_centroids[centroid_hits, , drop = FALSE] %>%
      semi_join(served_tracts %>% sf::st_drop_geometry() %>% dplyr::select(GEOID), by = c("tract_id" = "GEOID"))

    sf::st_write(served_tracts, served_tracts_path, delete_dsn = TRUE, quiet = TRUE)
    sf::st_write(served_centroids, served_centroids_path, delete_dsn = TRUE, quiet = TRUE)
  }

  invisible(service_area_sf)
}

get_active_geography_for_routing <- function(cfg) {
  served_tracts_path <- file.path(cfg$paths$service_area_dir, "tracts_served.gpkg")
  served_centroids_path <- file.path(cfg$paths$service_area_dir, "tract_centroids_served.gpkg")
  base <- read_geography_outputs(cfg)

  if (file_is_nonempty(served_tracts_path, 100) && file_is_nonempty(served_centroids_path, 100)) {
    served_tracts <- sf::st_read(served_tracts_path, quiet = TRUE) %>% standardize_tract_id_cols(c("GEOID"))
    served_centroids <- sf::st_read(served_centroids_path, quiet = TRUE) %>% standardize_tract_id_cols(c("tract_id"))
    served_zone_ids <- base$tract_to_zone %>%
      filter(tract_id %in% served_tracts$GEOID) %>%
      pull(zone_id) %>%
      unique()
    if (length(served_zone_ids) == 0) served_zone_ids <- unique(base$analysis_zone_centroids$zone_id)

    out <- list(
      tracts = served_tracts,
      tract_centroids = served_centroids,
      analysis_zones = base$analysis_zones %>% filter(zone_id %in% served_zone_ids),
      analysis_zone_centroids = base$analysis_zone_centroids %>% filter(zone_id %in% served_zone_ids),
      tract_to_zone = base$tract_to_zone %>% filter(zone_id %in% served_zone_ids)
    )
    out$routing_zone_centroids <- build_zone_centroids_for_routing(out, cfg)
    return(out)
  }

  base <- read_geography_outputs(cfg)
  out <- list(
    tracts = base$tracts,
    tract_centroids = base$tract_centroids,
    analysis_zones = base$analysis_zones,
    analysis_zone_centroids = base$analysis_zone_centroids,
    tract_to_zone = base$tract_to_zone
  )
  out$routing_zone_centroids <- build_zone_centroids_for_routing(out, cfg)
  out
}

build_zone_centroids_for_routing <- function(geography_outputs, cfg) {
  unit <- normalize_analysis_unit(cfg$geography$analysis_unit)
  zone_centroids_default <- geography_outputs$analysis_zone_centroids %>%
    mutate(zone_id = standardize_zone_id(zone_id, cfg$geography$analysis_unit)) %>%
    sf::st_transform(4326)

  if (unit != "zip") {
    return(zone_centroids_default)
  }

  tract_centroids <- geography_outputs$tract_centroids %>%
    standardize_tract_id_cols(c("tract_id")) %>%
    sf::st_transform(4326) %>%
    mutate(
      lon = sf::st_coordinates(.)[, 1],
      lat = sf::st_coordinates(.)[, 2]
    ) %>%
    sf::st_drop_geometry() %>%
    transmute(tract_id = standardize_geoid11(tract_id), lon, lat)

  tract_to_zone <- geography_outputs$tract_to_zone %>%
    transmute(tract_id = standardize_geoid11(tract_id), zone_id = standardize_zone_id(zone_id, cfg$geography$analysis_unit))

  zone_points_tbl <- tract_to_zone %>%
    inner_join(tract_centroids, by = "tract_id") %>%
    filter(!is.na(zone_id), !is.na(lon), !is.na(lat)) %>%
    group_by(zone_id) %>%
    group_modify(~ {
      cx <- mean(.x$lon, na.rm = TRUE)
      cy <- mean(.x$lat, na.rm = TRUE)
      d2 <- (.x$lon - cx)^2 + (.x$lat - cy)^2
      idx <- which.min(d2)
      tibble(
        lon = .x$lon[[idx]],
        lat = .x$lat[[idx]],
        n_tracts = nrow(.x),
        point_method = "nearest_tract_centroid_to_zone_mean",
        representative_tract_id = .x$tract_id[[idx]]
      )
    }) %>%
    ungroup()

  zone_sf <- sf::st_as_sf(zone_points_tbl, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

  missing_zone_ids <- setdiff(unique(zone_centroids_default$zone_id), unique(zone_sf$zone_id))
  if (length(missing_zone_ids) > 0) {
    fallback <- zone_centroids_default %>%
      filter(zone_id %in% missing_zone_ids) %>%
      mutate(
        lon = sf::st_coordinates(.)[, 1],
        lat = sf::st_coordinates(.)[, 2],
        n_tracts = NA_integer_,
        point_method = "fallback_zone_point_on_surface",
        representative_tract_id = NA_character_
      ) %>%
      dplyr::select(zone_id, lon, lat, n_tracts, point_method, representative_tract_id)
    zone_sf <- dplyr::bind_rows(zone_sf, fallback)
  }

  if (!is.null(cfg$paths$logs_dir)) {
    fs::dir_create(cfg$paths$logs_dir)
    qc_tbl <- zone_sf %>%
      sf::st_drop_geometry() %>%
      transmute(
        zone_id,
        analysis_unit = unit,
        lon,
        lat,
        n_tracts,
        point_method,
        representative_tract_id,
        is_fallback = point_method == "fallback_zone_point_on_surface"
      )
    readr::write_csv(qc_tbl, file.path(cfg$paths$logs_dir, "zone_point_qc.csv"))
  }

  zone_sf
}

choose_routing_dates <- function(cfg) {
  feed_registry <- make_feed_registry_for_routing(cfg)
  periods_tbl <- cfg$study_periods

  all_dates <- build_analysis_dates(
    periods = periods_tbl,
    weekday_only = cfg$analysis_calendar$weekday_only,
    excluded_dates = cfg$analysis_calendar$excluded_dates,
    feed_registry = feed_registry
  )

  if (identical(cfg$routing$date_strategy, "sample_n_dates_per_period")) {
    return(sample_dates_evenly(all_dates, cfg$routing$sample_n_dates_per_period))
  }

  all_dates
}

make_routing_points <- function(zone_centroids_sf, zone_ids, cfg) {
  zone_ids <- standardize_zone_id(zone_ids, cfg$geography$analysis_unit)
  zone_centroids_sf %>%
    mutate(zone_id = standardize_zone_id(zone_id, cfg$geography$analysis_unit)) %>%
    filter(zone_id %in% zone_ids) %>%
    sf::st_transform(4326) %>%
    mutate(lon = sf::st_coordinates(.)[, 1], lat = sf::st_coordinates(.)[, 2]) %>%
    sf::st_drop_geometry() %>%
    transmute(id = as.character(zone_id), lon = lon, lat = lat)
}

compute_ttm_one_chunk <- function(network, origins_df, destinations_df, departure_datetime, routing_cfg, window_minutes) {
  r5r::travel_time_matrix(
    r5r_network = network,
    origins = origins_df,
    destinations = destinations_df,
    mode = unlist(routing_cfg$modes),
    departure_datetime = departure_datetime,
    time_window = as.integer(window_minutes),
    percentiles = unlist(routing_cfg$percentiles),
    max_walk_time = routing_cfg$max_walk_time,
    max_trip_duration = routing_cfg$max_trip_duration,
    walk_speed = routing_cfg$walk_speed,
    max_rides = routing_cfg$max_rides,
    n_threads = routing_cfg$n_threads,
    progress = TRUE
  ) %>%
    mutate(
      from_id = as.character(from_id),
      to_id = as.character(to_id)
    )
}

compute_ttm_one_chunk_with_retry <- function(network, origins_df, destinations_df, departure_datetime, routing_cfg, window_minutes, cfg = NULL, context = list()) {
  safe_error_message <- function(err_obj) {
    msg <- tryCatch(base::conditionMessage(err_obj), error = function(...) NA_character_)
    if (is.na(msg) || !nzchar(msg)) {
      msg <- tryCatch(paste0(err_obj), error = function(...) NA_character_)
    }
    if (is.na(msg) || !nzchar(msg)) {
      msg <- tryCatch(paste(capture.output(print(err_obj)), collapse = " "), error = function(...) "unknown error")
    }
    msg
  }

  is_retryable_java_error <- function(msg_txt) {
    stringr::str_detect(msg_txt %||% "", "ArrayIndexOutOfBoundsException|ExecutionException")
  }

  empty_ttm_result <- function() {
    pct <- unlist(routing_cfg$percentiles %||% list(50))
    pct <- as.integer(pct[!is.na(pct)])
    out <- tibble::tibble(from_id = character(), to_id = character())
    for (p in pct) {
      out[[paste0("travel_time_p", p)]] <- numeric()
    }
    out
  }

  emit_routing_failure <- function(initial_msg, retry_msg = NULL, retry_attempted = FALSE) {
    detail <- list(
      failure_type = "routing_chunk_failure",
      feed_name = context$feed_name %||% NA_character_,
      analysis_date = as.character(context$analysis_date %||% NA_character_),
      period_id = as.character(context$period_id %||% NA_character_),
      period_label = as.character(context$period_label %||% NA_character_),
      time_window_id = as.character(context$time_window_id %||% NA_character_),
      od_scenario_id = as.character(context$od_scenario_id %||% NA_character_),
      origin_chunk_id = context$origin_chunk_id %||% NA_integer_,
      destination_chunk_id = context$destination_chunk_id %||% NA_integer_,
      origins_in_chunk = nrow(origins_df),
      destinations_in_chunk = nrow(destinations_df),
      od_pairs_in_chunk = context$od_pairs_in_chunk %||% NA_integer_,
      departure_datetime = as.character(departure_datetime),
      window_minutes = as.numeric(window_minutes),
      configured_n_threads = routing_cfg$n_threads %||% NA,
      retry_attempted = retry_attempted,
      retry_n_threads = if (retry_attempted) 1 else NA,
      initial_error = initial_msg,
      retry_error = retry_msg
    )

    if (!is.null(cfg) && !is.null(cfg$paths$logs_dir)) {
      fs::dir_create(cfg$paths$logs_dir)
      stamp <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")
      log_path <- file.path(cfg$paths$logs_dir, paste0("routing_error_", stamp, "_", context$time_window_id %||% "unknown", "_chunk", context$origin_chunk_id %||% "na", "_dest", context$destination_chunk_id %||% "na", ".json"))
      write_json_pretty(detail, log_path)
      detail$log_path <- log_path
    }

    msg_lines <- c(
      "Routing chunk failed with detailed context:",
      paste0("  feed_name: ", detail$feed_name),
      paste0("  analysis_date: ", detail$analysis_date, " (period ", detail$period_id, ")"),
      paste0("  time_window_id / od_scenario_id: ", detail$time_window_id, " / ", detail$od_scenario_id),
      paste0("  origin_chunk_id / destination_chunk_id: ", detail$origin_chunk_id, " / ", detail$destination_chunk_id),
      paste0("  origins_in_chunk / destinations_in_chunk / od_pairs_in_chunk: ", detail$origins_in_chunk, " / ", detail$destinations_in_chunk, " / ", detail$od_pairs_in_chunk),
      paste0("  departure_datetime: ", detail$departure_datetime, " (window_minutes=", detail$window_minutes, ")"),
      paste0("  configured_n_threads: ", detail$configured_n_threads, ", retry_attempted: ", detail$retry_attempted),
      paste0("  initial_error: ", detail$initial_error)
    )
    if (!is.null(retry_msg)) {
      msg_lines <- c(msg_lines, paste0("  retry_error: ", retry_msg))
    }
    if (!is.null(detail$log_path)) {
      msg_lines <- c(msg_lines, paste0("  detailed_log: ", detail$log_path))
    }

    stop(paste(msg_lines, collapse = "\n"), call. = FALSE)
  }

  tryCatch(
    compute_ttm_one_chunk(
      network = network,
      origins_df = origins_df,
      destinations_df = destinations_df,
      departure_datetime = departure_datetime,
      routing_cfg = routing_cfg,
      window_minutes = window_minutes
    ),
    error = function(e) {
      err_msg <- safe_error_message(e)
      if (is_retryable_java_error(err_msg)) {
        message("travel_time_matrix failed with Java execution error. Retrying with n_threads=1 for this chunk.")
        routing_cfg_retry <- routing_cfg
        routing_cfg_retry$n_threads <- 1
        retry_out <- tryCatch(
          compute_ttm_one_chunk(
            network = network,
            origins_df = origins_df,
            destinations_df = destinations_df,
            departure_datetime = departure_datetime,
            routing_cfg = routing_cfg_retry,
            window_minutes = window_minutes
          ),
          error = function(e_retry) {
            retry_msg <- safe_error_message(e_retry)
            if (!is_retryable_java_error(retry_msg)) {
              emit_routing_failure(initial_msg = err_msg, retry_msg = retry_msg, retry_attempted = TRUE)
            }

            message("Chunk still failing after n_threads=1 retry. Falling back to per-origin routing for this chunk.")

            origin_rows <- split(origins_df, seq_len(nrow(origins_df)))
            fallback_results <- vector("list", length(origin_rows))
            origin_failures <- vector("list", length(origin_rows))

            for (oi in seq_along(origin_rows)) {
              origin_one <- origin_rows[[oi]]
              origin_id <- origin_one$id[[1]]

              origin_try <- tryCatch(
                compute_ttm_one_chunk(
                  network = network,
                  origins_df = origin_one,
                  destinations_df = destinations_df,
                  departure_datetime = departure_datetime,
                  routing_cfg = routing_cfg_retry,
                  window_minutes = window_minutes
                ),
                error = function(e_origin) {
                  origin_msg <- safe_error_message(e_origin)

                  if (!is_retryable_java_error(origin_msg) || nrow(destinations_df) <= 1) {
                    origin_failures[[oi]] <<- list(origin_id = origin_id, error = origin_msg)
                    return(NULL)
                  }

                  sub_size <- max(1, min(25, nrow(destinations_df)))
                  dest_subchunks <- split(destinations_df, ceiling(seq_len(nrow(destinations_df)) / sub_size))
                  sub_parts <- vector("list", length(dest_subchunks))
                  sub_fail <- character()

                  for (si in seq_along(dest_subchunks)) {
                    sub_try <- tryCatch(
                      compute_ttm_one_chunk(
                        network = network,
                        origins_df = origin_one,
                        destinations_df = dest_subchunks[[si]],
                        departure_datetime = departure_datetime,
                        routing_cfg = routing_cfg_retry,
                        window_minutes = window_minutes
                      ),
                      error = function(e_sub) {
                        sub_fail <<- c(sub_fail, paste0("subchunk_", si, ": ", safe_error_message(e_sub)))
                        NULL
                      }
                    )
                    sub_parts[[si]] <- sub_try
                  }

                  sub_parts <- Filter(Negate(is.null), sub_parts)
                  if (length(sub_parts) == 0) {
                    origin_failures[[oi]] <<- list(
                      origin_id = origin_id,
                      error = paste(c(origin_msg, sub_fail), collapse = " | ")
                    )
                    return(NULL)
                  }
                  dplyr::bind_rows(sub_parts)
                }
              )

              fallback_results[[oi]] <- origin_try
            }

            fallback_results <- Filter(Negate(is.null), fallback_results)
            failed_origins <- Filter(Negate(is.null), origin_failures)

            if (length(failed_origins) > 0) {
              failed_ids <- vapply(failed_origins, function(x) as.character(x$origin_id %||% NA_character_), character(1))
              message("Per-origin fallback still failed for ", length(failed_origins), " origin(s): ", paste(failed_ids, collapse = ", "))
              if (!is.null(cfg) && !is.null(cfg$paths$logs_dir)) {
                fs::dir_create(cfg$paths$logs_dir)
                stamp2 <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")
                fallback_log <- list(
                  failure_type = "routing_chunk_partial_origin_failures",
                  context = context,
                  failed_origins = failed_origins
                )
                write_json_pretty(
                  fallback_log,
                  file.path(cfg$paths$logs_dir, paste0("routing_origin_fallback_", stamp2, "_", context$time_window_id %||% "unknown", "_chunk", context$origin_chunk_id %||% "na", "_dest", context$destination_chunk_id %||% "na", ".json"))
                )
              }
            }

            if (length(fallback_results) == 0) {
              msg_lines <- c(
                "All origins in this chunk failed after per-origin fallback. Returning empty chunk output; these pairs will be treated as unreachable downstream.",
                paste0("  feed_name: ", context$feed_name %||% "unknown"),
                paste0("  analysis_date: ", as.character(context$analysis_date %||% NA_character_)),
                paste0("  time_window_id / od_scenario_id: ", context$time_window_id %||% "unknown", " / ", context$od_scenario_id %||% "unknown"),
                paste0("  origin_chunk_id / destination_chunk_id: ", context$origin_chunk_id %||% NA, " / ", context$destination_chunk_id %||% NA),
                paste0("  initial_error: ", err_msg),
                paste0("  retry_error: ", retry_msg)
              )
              message(paste(msg_lines, collapse = "\n"))
              return(empty_ttm_result())
            }

            dplyr::bind_rows(fallback_results)
          }
        )
        return(retry_out)
      }
      emit_routing_failure(initial_msg = err_msg, retry_attempted = FALSE)
    }
  )
}

period_travel_time_output_path <- function(cfg) {
  file.path(cfg$paths$travel_time_dir, "period_travel_times.csv.gz")
}

expected_daily_output_paths <- function(cfg, od_all, geography_outputs, routing_dates, feed_name_filter = NULL) {
  windows_tbl <- tibble::as_tibble(dplyr::bind_rows(cfg$routing$routing_windows)) %>% mutate(time_window_id = as.character(time_window_id), od_scenario_id = as.character(od_scenario_id))
  out <- character()
  dates_use <- routing_dates
  if (!is.null(feed_name_filter)) {
    dates_use <- dates_use %>% filter(feed_name %in% feed_name_filter)
  }

  for (win_i in seq_len(nrow(windows_tbl))) {
    win <- windows_tbl[win_i, ]
    od_use <- od_all %>% filter(scenario_id == win$od_scenario_id)
    origin_ids <- unique(od_use$origin_id)
    origins_all <- make_routing_points(geography_outputs$routing_zone_centroids %||% geography_outputs$analysis_zone_centroids, origin_ids, cfg)
    n_chunks <- if (nrow(origins_all) == 0) 0 else ceiling(nrow(origins_all) / cfg$routing$origin_chunk_size)

    if (n_chunks == 0) {
      next
    }

    for (d_i in seq_len(nrow(dates_use))) {
      date_row <- dates_use[d_i, ]
      for (chunk_i in seq_len(n_chunks)) {
        out <- c(out, file.path(
          cfg$paths$travel_time_dir,
          "daily",
          paste0(
            cfg$project$city_id, "_",
            win$time_window_id, "_",
            date_row$period_id, "_",
            date_row$analysis_date, "_chunk", chunk_i, ".csv.gz"
          )
        ))
      }
    }
  }

  unique(out)
}

aggregate_period_travel_times <- function(cfg) {
  daily_dir <- file.path(cfg$paths$travel_time_dir, "daily")
  daily_files <- fs::dir_ls(daily_dir, glob = "*.csv.gz")
  out_path <- period_travel_time_output_path(cfg)

  if (!isTRUE(cfg$run_options$force_routing) && output_is_fresh(out_path, daily_files, min_bytes = 100)) {
    message("Using cached period travel times.")
    return(read_csv_guess(out_path) %>% mutate(from_id = standardize_zone_id(from_id, cfg$geography$analysis_unit), to_id = standardize_zone_id(to_id, cfg$geography$analysis_unit)))
  }

  if (length(daily_files) == 0) {
    stop("No daily travel time files found.", call. = FALSE)
  }

  daily_all <- purrr::map_dfr(daily_files, read_csv_guess) %>%
    mutate(
      from_id = standardize_zone_id(from_id, cfg$geography$analysis_unit),
      to_id = standardize_zone_id(to_id, cfg$geography$analysis_unit),
      analysis_date = as.Date(analysis_date),
      period_id = as.character(period_id),
      time_window_id = as.character(time_window_id),
      od_scenario_id = as.character(od_scenario_id)
    )

  if (nrow(daily_all) == 0) {
    stop("Daily travel time files were found, but they are empty.", call. = FALSE)
  }

  od_all <- read_od_weights(cfg) %>% select(scenario_id, origin_id, destination_id) %>% distinct()
  routing_dates <- choose_routing_dates(cfg) %>% select(period_id, period_label, analysis_date) %>% distinct()
  windows_tbl <- tibble::as_tibble(dplyr::bind_rows(cfg$routing$routing_windows)) %>% mutate(time_window_id = as.character(time_window_id), od_scenario_id = as.character(od_scenario_id)) %>% select(time_window_id, od_scenario_id)
  routing_grid <- routing_dates %>% tidyr::crossing(windows_tbl)

  complete_daily <- routing_grid %>%
    left_join(od_all, by = c("od_scenario_id" = "scenario_id")) %>%
    rename(from_id = origin_id, to_id = destination_id) %>%
    mutate(from_id = standardize_zone_id(from_id, cfg$geography$analysis_unit), to_id = standardize_zone_id(to_id, cfg$geography$analysis_unit)) %>%
    left_join(
      daily_all,
      by = c("period_id", "analysis_date", "time_window_id", "od_scenario_id", "from_id", "to_id")
    )

  value_cols <- names(daily_all)[stringr::str_detect(names(daily_all), "^travel_time_p")]

  period_agg <- complete_daily %>%
    group_by(period_id, time_window_id, od_scenario_id, from_id, to_id) %>%
    summarise(
      n_days_total = n_distinct(analysis_date),
      n_days_reached = sum(!is.na(travel_time_p50), na.rm = TRUE),
      share_days_unreachable = if_else(n_days_total > 0, 1 - (n_days_reached / n_days_total), NA_real_),
      across(all_of(value_cols), ~ mean_or_na(.x), .names = "{.col}"),
      across(all_of(value_cols), ~ mean_penalized(.x, cfg$routing$unreachable_penalty_minutes), .names = "{.col}_penalized"),
      .groups = "drop"
    )

  write_csv_gz(period_agg, out_path)
  invisible(period_agg)
}

compute_all_travel_times <- function(cfg) {
  fs::dir_create(file.path(cfg$paths$travel_time_dir, "daily"))

  od_all <- read_od_weights(cfg) %>%
    mutate(
      scenario_id = as.character(scenario_id),
      origin_id = as.character(standardize_zone_id(origin_id, cfg$geography$analysis_unit)),
      destination_id = as.character(standardize_zone_id(destination_id, cfg$geography$analysis_unit))
    )
  geography_outputs <- get_active_geography_for_routing(cfg)
  routing_dates <- choose_routing_dates(cfg)
  feed_registry <- make_feed_registry_for_routing(cfg)
  windows_tbl <- tibble::as_tibble(dplyr::bind_rows(cfg$routing$routing_windows)) %>% mutate(time_window_id = as.character(time_window_id), od_scenario_id = as.character(od_scenario_id))
  expected_files <- expected_daily_output_paths(cfg, od_all, geography_outputs, routing_dates)
  period_out <- period_travel_time_output_path(cfg)

  if (!isTRUE(cfg$run_options$force) && !isTRUE(cfg$run_options$force_routing) && length(expected_files) > 0 && all_files_nonempty(expected_files, min_bytes = 100) && output_is_fresh(period_out, expected_files, min_bytes = 100)) {
    message("All travel time chunks and period aggregates already exist. Skipping routing.")
    return(read_csv_guess(period_out) %>% mutate(from_id = standardize_zone_id(from_id, cfg$geography$analysis_unit), to_id = standardize_zone_id(to_id, cfg$geography$analysis_unit)))
  }

  feed_groups <- split(routing_dates, routing_dates$feed_name)

  for (feed_name in names(feed_groups)) {
    feed_expected <- expected_daily_output_paths(cfg, od_all, geography_outputs, routing_dates, feed_name_filter = feed_name)
    if (!isTRUE(cfg$run_options$force_routing) && length(feed_expected) > 0 && all_files_nonempty(feed_expected, min_bytes = 100)) {
      message("All routing chunks for ", feed_name, " already exist. Skipping network build for this feed.")
      next
    }

    gtfs_files <- feed_registry %>% filter(feed_name == !!feed_name) %>% pull(gtfs_files) %>% .[[1]]
    network <- NULL

    tryCatch({
      network <- build_r5_network_object(cfg, feed_name, gtfs_files)
      date_tbl <- feed_groups[[feed_name]]

      for (win_i in seq_len(nrow(windows_tbl))) {
        win <- windows_tbl[win_i, ]
        od_use <- od_all %>%
          filter(scenario_id == win$od_scenario_id) %>%
          mutate(
            origin_id = as.character(standardize_zone_id(origin_id, cfg$geography$analysis_unit)),
            destination_id = as.character(standardize_zone_id(destination_id, cfg$geography$analysis_unit))
          )

        origin_ids <- unique(od_use$origin_id)
        destination_ids <- unique(od_use$destination_id)

        origins_all <- make_routing_points(geography_outputs$routing_zone_centroids %||% geography_outputs$analysis_zone_centroids, origin_ids, cfg)
        destinations_all <- make_routing_points(geography_outputs$routing_zone_centroids %||% geography_outputs$analysis_zone_centroids, destination_ids, cfg)

        if (nrow(origins_all) == 0 || nrow(destinations_all) == 0) {
          next
        }

        origin_chunks <- split(origins_all, ceiling(seq_len(nrow(origins_all)) / cfg$routing$origin_chunk_size))

        for (d_i in seq_len(nrow(date_tbl))) {
          date_row <- date_tbl[d_i, ]
          departure_datetime <- combine_date_time(date_row$analysis_date, win$start_time, cfg$project$timezone)
          time_window_minutes <- window_minutes(win$start_time, win$end_time)

          for (chunk_i in seq_along(origin_chunks)) {
            out_path <- file.path(
              cfg$paths$travel_time_dir,
              "daily",
              paste0(
                cfg$project$city_id, "_",
                win$time_window_id, "_",
                date_row$period_id, "_",
                date_row$analysis_date, "_chunk", chunk_i, ".csv.gz"
              )
            )

            if (!isTRUE(cfg$run_options$force_routing) && file_is_nonempty(out_path, min_bytes = 100)) {
              next
            }

            origins_chunk <- origin_chunks[[chunk_i]]
            od_pairs_chunk <- od_use %>%
              filter(origin_id %in% origins_chunk$id) %>%
              transmute(
                origin_id = as.character(standardize_zone_id(origin_id, cfg$geography$analysis_unit)),
                destination_id = as.character(standardize_zone_id(destination_id, cfg$geography$analysis_unit))
              ) %>%
              distinct()

            if (nrow(od_pairs_chunk) == 0) {
              next
            }

            destinations_needed <- destinations_all %>%
              filter(id %in% unique(od_pairs_chunk$destination_id))

            if (nrow(destinations_needed) == 0) {
              next
            }

            destination_chunk_size <- cfg$routing$destination_chunk_size %||% 200
            destination_chunks <- split(destinations_needed, ceiling(seq_len(nrow(destinations_needed)) / destination_chunk_size))

            ttm_parts <- purrr::imap(destination_chunks, function(dest_chunk, dest_chunk_i) {
              compute_ttm_one_chunk_with_retry(
                network = network,
                origins_df = origins_chunk,
                destinations_df = dest_chunk,
                departure_datetime = departure_datetime,
                routing_cfg = cfg$routing,
                window_minutes = time_window_minutes,
                cfg = cfg,
                context = list(
                  feed_name = date_row$feed_name,
                  analysis_date = date_row$analysis_date,
                  period_id = date_row$period_id,
                  period_label = date_row$period_label,
                  time_window_id = win$time_window_id,
                  od_scenario_id = win$od_scenario_id,
                  origin_chunk_id = chunk_i,
                  destination_chunk_id = as.integer(dest_chunk_i),
                  od_pairs_in_chunk = nrow(od_pairs_chunk)
                )
              )
            })
            ttm <- dplyr::bind_rows(ttm_parts)

            out <- ttm %>%
              mutate(from_id = as.character(from_id), to_id = as.character(to_id)) %>%
              inner_join(
                od_pairs_chunk %>% mutate(origin_id = as.character(origin_id), destination_id = as.character(destination_id)),
                by = c("from_id" = "origin_id", "to_id" = "destination_id")
              ) %>%
              mutate(
                period_id = as.character(date_row$period_id),
                period_label = as.character(date_row$period_label),
                analysis_date = as.Date(date_row$analysis_date),
                feed_name = as.character(date_row$feed_name),
                time_window_id = as.character(win$time_window_id),
                od_scenario_id = as.character(win$od_scenario_id),
                origin_chunk_id = chunk_i
              )

            write_csv_gz(out, out_path)
          }
        }
      }
    }, finally = {
      if (!is.null(network) && inherits(network, "r5r_network")) {
        try(r5r::stop_r5(network), silent = TRUE)
      }
      if (requireNamespace("rJava", quietly = TRUE)) {
        try(rJava::.jgc(R.gc = TRUE), silent = TRUE)
      }
      invisible(gc())
    })
  }

  aggregate_period_travel_times(cfg)
}
