compute_period_metrics <- function(cfg) {
  od_all <- read_od_weights(cfg) %>%
    mutate(
      scenario_id = as.character(scenario_id),
      time_bin = as.character(time_bin)
    )

  period_ttm <- read_csv_guess(file.path(cfg$paths$travel_time_dir, "period_travel_times.csv.gz")) %>%
    mutate(
      from_id = standardize_zone_id(from_id, cfg$geography$analysis_unit),
      to_id = standardize_zone_id(to_id, cfg$geography$analysis_unit),
      od_scenario_id = as.character(od_scenario_id),
      time_window_id = as.character(time_window_id),
      period_id = as.character(period_id)
    )

  geography_outputs <- read_geography_outputs(cfg)

  od_joined <- od_all %>%
    left_join(
      period_ttm,
      by = c("scenario_id" = "od_scenario_id", "origin_id" = "from_id", "destination_id" = "to_id", "time_bin" = "time_window_id")
    ) %>%
    mutate(
      pair_share_days_unreachable = dplyr::coalesce(share_days_unreachable, 1),
      unreachable_flag = pair_share_days_unreachable >= 1,
      travel_time_penalized = dplyr::coalesce(travel_time_p50_penalized, cfg$routing$unreachable_penalty_minutes),
      weight_for_metrics = dplyr::coalesce(weight_sum_adjusted, weight_sum),
      reachable_weight_equiv = weight_for_metrics * (1 - pair_share_days_unreachable)
    ) %>%
    filter(
      !is.na(period_id),
      nzchar(period_id),
      !is.na(time_bin),
      nzchar(time_bin)
    )

  tract_period_metrics <- od_joined %>%
    group_by(origin_id, scenario_id, time_window_id = time_bin, period_id) %>%
    summarise(
      total_od_weight = sum(weight_for_metrics, na.rm = TRUE),
      reachable_od_weight_equiv = sum(reachable_weight_equiv, na.rm = TRUE),
      avg_weighted_travel_time_reachable = if_else(
        reachable_od_weight_equiv > 0,
        sum(travel_time_p50 * reachable_weight_equiv, na.rm = TRUE) / reachable_od_weight_equiv,
        NA_real_
      ),
      avg_weighted_travel_time_penalized = if_else(
        total_od_weight > 0,
        sum(travel_time_penalized * weight_for_metrics, na.rm = TRUE) / total_od_weight,
        NA_real_
      ),
      share_unreachable_weighted = if_else(
        total_od_weight > 0,
        sum(weight_for_metrics * pair_share_days_unreachable, na.rm = TRUE) / total_od_weight,
        NA_real_
      ),
      share_accessible_45min_weighted = if_else(
        total_od_weight > 0,
        sum(weight_for_metrics * as.numeric(travel_time_penalized <= 45), na.rm = TRUE) / total_od_weight,
        NA_real_
      ),
      share_accessible_60min_weighted = if_else(
        total_od_weight > 0,
        sum(weight_for_metrics * as.numeric(travel_time_penalized <= 60), na.rm = TRUE) / total_od_weight,
        NA_real_
      ),
      .groups = "drop"
    )

  pair_period_metrics <- od_joined %>%
    transmute(
      origin_id = standardize_zone_id(origin_id, cfg$geography$analysis_unit),
      destination_id = standardize_zone_id(destination_id, cfg$geography$analysis_unit),
      scenario_id,
      time_window_id = time_bin,
      period_id,
      weight_for_metrics,
      pair_share_days_unreachable,
      unreachable_flag,
      travel_time_penalized
    )

  comparison_tbl <- add_period_comparison_ids(unique(tract_period_metrics$period_id))

  tract_period_comparisons <- comparison_tbl %>%
    left_join(
      tract_period_metrics %>% rename(base_period_id = period_id),
      by = "base_period_id"
    ) %>%
    left_join(
      tract_period_metrics %>% rename(compare_period_id = period_id),
      by = c("compare_period_id", "origin_id", "scenario_id", "time_window_id"),
      suffix = c("_base", "_compare")
    ) %>%
    transmute(
      comparison_id,
      base_period_id,
      compare_period_id,
      origin_id,
      scenario_id,
      time_window_id,
      avg_weighted_travel_time_penalized_base = avg_weighted_travel_time_penalized_base,
      avg_weighted_travel_time_penalized_compare = avg_weighted_travel_time_penalized_compare,
      share_unreachable_weighted_base = share_unreachable_weighted_base,
      share_unreachable_weighted_compare = share_unreachable_weighted_compare,
      delta_avg_weighted_travel_time_penalized = avg_weighted_travel_time_penalized_compare - avg_weighted_travel_time_penalized_base,
      delta_pct_weighted_travel_time_penalized = if_else(
        !is.na(avg_weighted_travel_time_penalized_base) & avg_weighted_travel_time_penalized_base != 0,
        100 * (avg_weighted_travel_time_penalized_compare - avg_weighted_travel_time_penalized_base) / avg_weighted_travel_time_penalized_base,
        NA_real_
      ),
      delta_unreachable_share_weighted = share_unreachable_weighted_compare - share_unreachable_weighted_base
    )

  pair_comparisons <- comparison_tbl %>%
    left_join(pair_period_metrics %>% rename(base_period_id = period_id), by = "base_period_id") %>%
    left_join(
      pair_period_metrics %>% rename(compare_period_id = period_id),
      by = c("compare_period_id", "origin_id", "destination_id", "scenario_id", "time_window_id"),
      suffix = c("_base", "_compare")
    ) %>%
    mutate(
      delta_minutes_pair = travel_time_penalized_compare - travel_time_penalized_base,
      becomes_unreachable = (!unreachable_flag_base) & unreachable_flag_compare,
      increase_gt_10 = delta_minutes_pair > 10
    )

  tract_pair_comparison_metrics <- pair_comparisons %>%
    group_by(comparison_id, base_period_id, compare_period_id, origin_id, scenario_id, time_window_id) %>%
    summarise(
      total_weight_pairs = sum(weight_for_metrics_base, na.rm = TRUE),
      share_weight_increase_gt_10 = if_else(
        total_weight_pairs > 0,
        sum(weight_for_metrics_base * as.numeric(increase_gt_10), na.rm = TRUE) / total_weight_pairs,
        NA_real_
      ),
      share_weight_becomes_unreachable = if_else(
        total_weight_pairs > 0,
        sum(weight_for_metrics_base * as.numeric(becomes_unreachable), na.rm = TRUE) / total_weight_pairs,
        NA_real_
      ),
      .groups = "drop"
    )

  tract_period_comparisons <- tract_period_comparisons %>%
    left_join(
      tract_pair_comparison_metrics,
      by = c("comparison_id", "base_period_id", "compare_period_id", "origin_id", "scenario_id", "time_window_id")
    )

  analysis_zones_sf <- geography_outputs$analysis_zones %>% rename(origin_id = zone_id, NAME = zone_name) %>% mutate(origin_id = standardize_zone_id(origin_id, cfg$geography$analysis_unit))
  tract_period_metrics_sf <- analysis_zones_sf %>% left_join(tract_period_metrics, by = "origin_id")
  tract_period_comparisons_sf <- analysis_zones_sf %>% left_join(tract_period_comparisons, by = "origin_id")

  fs::dir_create(cfg$paths$accessibility_dir)
  readr::write_csv(tract_period_metrics, file.path(cfg$paths$accessibility_dir, "tract_period_metrics.csv"))
  readr::write_csv(tract_period_comparisons, file.path(cfg$paths$accessibility_dir, "tract_period_comparisons.csv"))
  sf::st_write(tract_period_metrics_sf, file.path(cfg$paths$accessibility_dir, "tract_period_metrics.gpkg"), delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(tract_period_comparisons_sf, file.path(cfg$paths$accessibility_dir, "tract_period_comparisons.gpkg"), delete_dsn = TRUE, quiet = TRUE)

  invisible(list(metrics = tract_period_metrics, comparisons = tract_period_comparisons))
}
