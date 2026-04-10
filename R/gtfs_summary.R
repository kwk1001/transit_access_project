read_gtfs_registry <- function(cfg) {
  tibble(
    feed_name = purrr::map_chr(cfg$gtfs_feeds, "feed_name"),
    valid_start = as.Date(purrr::map(cfg$gtfs_feeds, "valid_start") %>% unlist()),
    valid_end_exclusive = as.Date(purrr::map(cfg$gtfs_feeds, "valid_end_exclusive") %>% unlist()),
    gtfs_files = purrr::map(cfg$gtfs_feeds, "gtfs_files")
  )
}

load_gtfs_registry_objects <- function(feed_registry) {
  feed_registry %>%
    mutate(gt = purrr::map(gtfs_files, ~ tidytransit::read_gtfs(.x[[1]])))
}

get_gt_by_name <- function(feed_name, registry) {
  registry$gt[[match(feed_name, registry$feed_name)]]
}

get_active_ids <- function(gt, target_date) {
  gt$.$dates_services %>%
    filter(date == as.Date(target_date)) %>%
    distinct(service_id) %>%
    pull(service_id)
}

get_route_frequency_one_day <- function(feed_name, target_date, period_id, period_label, start_time, end_time, time_window_id, registry) {
  gt <- get_gt_by_name(feed_name, registry)
  active_ids <- get_active_ids(gt, target_date)

  if (length(active_ids) == 0) {
    return(tibble())
  }

  gt %>%
    tidytransit::get_route_frequency(start_time = start_time, end_time = end_time, service_ids = active_ids) %>%
    mutate(
      period_id = period_id,
      period_label = period_label,
      analysis_date = as.Date(target_date),
      feed_name = feed_name,
      time_window_id = time_window_id
    )
}

get_trip_counts_one_day <- function(feed_name, target_date, period_id, period_label, start_time, end_time, time_window_id, registry) {
  gt <- get_gt_by_name(feed_name, registry)
  active_ids <- get_active_ids(gt, target_date)

  if (length(active_ids) == 0) {
    return(tibble())
  }

  start_sec <- hms_to_seconds(start_time)
  end_sec <- hms_to_seconds(end_time)

  gt$stop_times %>%
    mutate(dep_sec = hms_to_seconds(departure_time)) %>%
    filter(dep_sec >= start_sec, dep_sec < end_sec) %>%
    inner_join(gt$trips, by = "trip_id") %>%
    filter(service_id %in% active_ids) %>%
    inner_join(gt$routes %>% select(route_id), by = "route_id") %>%
    distinct(route_id, direction_id, trip_id) %>%
    count(route_id, direction_id, name = "n_trips") %>%
    mutate(
      period_id = period_id,
      period_label = period_label,
      analysis_date = as.Date(target_date),
      feed_name = feed_name,
      time_window_id = time_window_id
    )
}

summarize_gtfs_by_period <- function(cfg) {
  feed_registry <- read_gtfs_registry(cfg)
  gtfs_registry <- load_gtfs_registry_objects(feed_registry)
  periods_tbl <- cfg$study_periods
  windows_tbl <- tibble::as_tibble(dplyr::bind_rows(cfg$routing$routing_windows))

  analysis_dates <- build_analysis_dates(
    periods = periods_tbl,
    weekday_only = cfg$analysis_calendar$weekday_only,
    excluded_dates = cfg$analysis_calendar$excluded_dates,
    feed_registry = feed_registry
  ) %>%
    mutate(
      n_active_ids = purrr::map2_int(feed_name, analysis_date, ~ length(get_active_ids(get_gt_by_name(.x, gtfs_registry), .y)))
    ) %>%
    filter(n_active_ids > 0)

  route_lookup <- purrr::map_dfr(gtfs_registry$gt, ~ .x$routes %>% select(route_id, route_short_name, route_long_name, route_type)) %>%
    distinct(route_id, .keep_all = TRUE)

  daily_freq <- purrr::map_dfr(seq_len(nrow(windows_tbl)), function(i) {
    win <- windows_tbl[i, ]
    purrr::map_dfr(seq_len(nrow(analysis_dates)), function(j) {
      row_i <- analysis_dates[j, ]
      get_route_frequency_one_day(
        feed_name = row_i$feed_name,
        target_date = row_i$analysis_date,
        period_id = row_i$period_id,
        period_label = row_i$period_label,
        start_time = win$start_time,
        end_time = win$end_time,
        time_window_id = win$time_window_id,
        registry = gtfs_registry
      )
    })
  })

  daily_trips <- purrr::map_dfr(seq_len(nrow(windows_tbl)), function(i) {
    win <- windows_tbl[i, ]
    purrr::map_dfr(seq_len(nrow(analysis_dates)), function(j) {
      row_i <- analysis_dates[j, ]
      get_trip_counts_one_day(
        feed_name = row_i$feed_name,
        target_date = row_i$analysis_date,
        period_id = row_i$period_id,
        period_label = row_i$period_label,
        start_time = win$start_time,
        end_time = win$end_time,
        time_window_id = win$time_window_id,
        registry = gtfs_registry
      )
    })
  })

  period_meta <- analysis_dates %>%
    group_by(period_id, period_label) %>%
    summarise(
      n_analysis_days = n(),
      feeds_used = paste(sort(unique(feed_name)), collapse = "; "),
      .groups = "drop"
    )

  freq_day_summary <- daily_freq %>%
    group_by(period_id, period_label, analysis_date, feed_name, time_window_id) %>%
    summarise(
      n_routes = n_distinct(route_id),
      median_route_headway_min = median(median_headways, na.rm = TRUE) / 60,
      mean_route_headway_min = mean(median_headways, na.rm = TRUE) / 60,
      .groups = "drop"
    )

  trip_day_summary <- daily_trips %>%
    group_by(period_id, period_label, analysis_date, feed_name, time_window_id) %>%
    summarise(
      total_trips = sum(n_trips, na.rm = TRUE),
      n_route_direction = n(),
      .groups = "drop"
    )

  period_summary <- period_meta %>%
    left_join(
      freq_day_summary %>%
        group_by(period_id, period_label, time_window_id) %>%
        summarise(
          avg_n_routes = mean(n_routes, na.rm = TRUE),
          avg_median_route_headway_min = mean(median_route_headway_min, na.rm = TRUE),
          avg_mean_route_headway_min = mean(mean_route_headway_min, na.rm = TRUE),
          .groups = "drop"
        ),
      by = c("period_id", "period_label")
    ) %>%
    left_join(
      trip_day_summary %>%
        group_by(period_id, period_label, time_window_id) %>%
        summarise(
          avg_total_trips = mean(total_trips, na.rm = TRUE),
          avg_n_route_direction = mean(n_route_direction, na.rm = TRUE),
          .groups = "drop"
        ),
      by = c("period_id", "period_label", "time_window_id")
    )

  period_sizes <- analysis_dates %>% group_by(period_id) %>% summarise(n_days_in_period = n(), .groups = "drop")

  headway_period_avg <- daily_freq %>%
    group_by(period_id, time_window_id, route_id) %>%
    summarise(
      days_present = n_distinct(analysis_date),
      avg_headway_min = mean(median_headways, na.rm = TRUE) / 60,
      median_headway_min = median(median_headways, na.rm = TRUE) / 60,
      .groups = "drop"
    ) %>%
    left_join(period_sizes, by = "period_id") %>%
    mutate(share_days_present = days_present / n_days_in_period) %>%
    left_join(route_lookup, by = "route_id")

  route_dir_lookup <- daily_trips %>% select(route_id, direction_id, time_window_id) %>% distinct()
  date_route_dir_grid <- analysis_dates %>% select(period_id, period_label, analysis_date) %>% distinct() %>% tidyr::crossing(route_dir_lookup)

  trip_daily_complete <- date_route_dir_grid %>%
    left_join(
      daily_trips %>% select(period_id, analysis_date, route_id, direction_id, time_window_id, n_trips),
      by = c("period_id", "analysis_date", "route_id", "direction_id", "time_window_id")
    ) %>%
    mutate(n_trips = tidyr::replace_na(n_trips, 0L))

  trip_period_avg <- trip_daily_complete %>%
    group_by(period_id, time_window_id, route_id, direction_id) %>%
    summarise(
      avg_n_trips = mean(n_trips, na.rm = TRUE),
      median_n_trips = median(n_trips, na.rm = TRUE),
      days_with_service = sum(n_trips > 0, na.rm = TRUE),
      share_days_with_service = mean(n_trips > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(route_lookup, by = "route_id")

  comparison_ids <- add_period_comparison_ids(periods_tbl$period_id)

  headway_compare <- headway_period_avg %>%
    select(period_id, time_window_id, route_id, route_short_name, route_long_name, avg_headway_min, share_days_present) %>%
    pivot_wider(names_from = period_id, values_from = c(avg_headway_min, share_days_present), names_glue = "{period_id}_{.value}")

  for (i in seq_len(nrow(comparison_ids))) {
    row_i <- comparison_ids[i, ]
    lhs <- paste0(row_i$compare_period_id, "_avg_headway_min")
    rhs <- paste0(row_i$base_period_id, "_avg_headway_min")
    new_col <- paste0(row_i$comparison_id, "_diff_headway_min")
    headway_compare[[new_col]] <- headway_compare[[lhs]] - headway_compare[[rhs]]
  }

  trip_compare <- trip_period_avg %>%
    select(period_id, time_window_id, route_id, direction_id, route_short_name, route_long_name, avg_n_trips, share_days_with_service) %>%
    pivot_wider(names_from = period_id, values_from = c(avg_n_trips, share_days_with_service), names_glue = "{period_id}_{.value}")

  for (i in seq_len(nrow(comparison_ids))) {
    row_i <- comparison_ids[i, ]
    lhs <- paste0(row_i$compare_period_id, "_avg_n_trips")
    rhs <- paste0(row_i$base_period_id, "_avg_n_trips")
    new_col <- paste0(row_i$comparison_id, "_diff_trips")
    trip_compare[[new_col]] <- trip_compare[[lhs]] - trip_compare[[rhs]]
  }

  fs::dir_create(cfg$paths$gtfs_output_dir)
  readr::write_csv(analysis_dates, file.path(cfg$paths$gtfs_output_dir, "analysis_dates_used.csv"))
  readr::write_csv(freq_day_summary, file.path(cfg$paths$gtfs_output_dir, "route_frequency_day_summary.csv"))
  readr::write_csv(trip_day_summary, file.path(cfg$paths$gtfs_output_dir, "trip_day_summary.csv"))
  readr::write_csv(period_summary, file.path(cfg$paths$gtfs_output_dir, "period_summary.csv"))
  readr::write_csv(headway_period_avg, file.path(cfg$paths$gtfs_output_dir, "headway_period_avg.csv"))
  readr::write_csv(trip_period_avg, file.path(cfg$paths$gtfs_output_dir, "trip_period_avg.csv"))
  readr::write_csv(headway_compare, file.path(cfg$paths$gtfs_output_dir, "headway_compare.csv"))
  readr::write_csv(trip_compare, file.path(cfg$paths$gtfs_output_dir, "trip_compare.csv"))

  invisible(list(
    analysis_dates = analysis_dates,
    period_summary = period_summary,
    headway_compare = headway_compare,
    trip_compare = trip_compare,
    headway_period_avg = headway_period_avg,
    trip_period_avg = trip_period_avg
  ))
}
