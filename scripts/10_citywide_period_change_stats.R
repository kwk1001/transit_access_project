`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

required_pkgs <- c("readr", "dplyr", "tidyr", "stringr", "yaml", "fs", "tibble")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    paste0(
      "Missing required packages: ",
      paste(missing_pkgs, collapse = ", "),
      ". Install them before running this script."
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(yaml)
  library(fs)
  library(tibble)
})

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

normalize_analysis_unit <- function(unit) {
  unit_chr <- tolower(trimws(as.character(unit %||% "tract")))
  unit_chr <- stringr::str_replace_all(unit_chr, "[-\\s]+", "_")
  dplyr::case_when(
    unit_chr %in% c("tract", "census_tract") ~ "tract",
    unit_chr %in% c("zip", "zipcode", "zip_code", "zcta") ~ "zip",
    unit_chr == "taz" ~ "taz",
    TRUE ~ unit_chr
  )
}

standardize_geoid11 <- function(x) {
  x_chr <- as.character(x)
  x_chr <- stringr::str_replace_all(x_chr, "[^0-9]", "")
  x_chr[x_chr == ""] <- NA_character_
  ifelse(is.na(x_chr), NA_character_, stringr::str_pad(x_chr, width = 11, side = "left", pad = "0"))
}

standardize_zone_id <- function(x, unit = "tract") {
  unit_use <- normalize_analysis_unit(unit)
  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA_character_

  if (unit_use == "tract") {
    return(standardize_geoid11(x_chr))
  }

  if (unit_use == "zip") {
    digits <- stringr::str_replace_all(x_chr, "[^0-9]", "")
    digits[digits == ""] <- NA_character_
    return(ifelse(is.na(digits), NA_character_, stringr::str_pad(digits, width = 5, side = "left", pad = "0")))
  }

  x_chr
}

read_csv_guess <- function(path, ...) {
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE, guess_max = 100000, ...)
}

is_run_root_dir <- function(path) {
  dir.exists(path) && (
    dir.exists(file.path(path, "accessibility")) ||
      (dir.exists(file.path(path, "od")) && dir.exists(file.path(path, "travel_times")))
  )
}

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

parse_cli_args <- function(args) {
  out <- list(
    config = NULL,
    run_root = NULL,
    source_id = NULL,
    analysis_unit = NULL,
    run_id = NULL,
    scenario_id = NULL,
    time_window_id = NULL,
    output_dir = NULL
  )

  named <- args[startsWith(args, "--")]
  positional <- args[!startsWith(args, "--")]

  if (length(named) > 0) {
    for (arg in named) {
      key_val <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1]]
      key <- key_val[[1]]
      val <- if (length(key_val) >= 2) paste(key_val[-1], collapse = "=") else ""
      if (key == "config") out$config <- val
      if (key == "run-root") out$run_root <- val
      if (key == "source-id") out$source_id <- val
      if (key == "analysis-unit") out$analysis_unit <- val
      if (key == "run-id") out$run_id <- val
      if (key == "scenario-id") out$scenario_id <- val
      if (key == "time-window-id") out$time_window_id <- val
      if (key == "output-dir") out$output_dir <- val
    }
  }

  if (length(positional) >= 1) {
    first <- positional[[1]]
    if (grepl("\\.(yml|yaml)$", first, ignore.case = TRUE)) {
      out$config <- out$config %||% first
      if (length(positional) >= 2) out$source_id <- out$source_id %||% positional[[2]]
      if (length(positional) >= 3) out$analysis_unit <- out$analysis_unit %||% positional[[3]]
      if (length(positional) >= 4) out$run_id <- out$run_id %||% positional[[4]]
      if (length(positional) >= 5) out$scenario_id <- out$scenario_id %||% positional[[5]]
      if (length(positional) >= 6) out$time_window_id <- out$time_window_id %||% positional[[6]]
    } else {
      out$run_root <- out$run_root %||% first
      if (length(positional) >= 2) out$scenario_id <- out$scenario_id %||% positional[[2]]
      if (length(positional) >= 3) out$time_window_id <- out$time_window_id %||% positional[[3]]
      if (length(positional) >= 4) out$analysis_unit <- out$analysis_unit %||% positional[[4]]
    }
  }

  out
}

find_latest_run_root <- function(run_base_dir) {
  run_dirs <- fs::dir_ls(run_base_dir, type = "directory", recurse = FALSE)
  if (length(run_dirs) == 0) {
    stop(paste0("No run folders found under: ", run_base_dir), call. = FALSE)
  }
  info <- file.info(run_dirs)
  run_dirs[[which.max(info$mtime)]]
}

weighted_mean_safe <- function(x, w) {
  x_num <- safe_numeric(x)
  w_num <- safe_numeric(w)
  ok <- is.finite(x_num) & is.finite(w_num) & w_num > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x_num[ok], w_num[ok], na.rm = TRUE)
}

weighted_paired_t_test <- function(diff_vec, weight_vec, conf_level = 0.95) {
  diff_num <- safe_numeric(diff_vec)
  w_num <- safe_numeric(weight_vec)
  ok <- is.finite(diff_num) & is.finite(w_num) & w_num > 0

  diff_num <- diff_num[ok]
  w_num <- w_num[ok]

  if (length(diff_num) == 0) {
    return(list(
      estimate = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      parameter = NA_real_,
      p_value = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_,
      n_obs = 0L,
      n_eff = NA_real_,
      weight_sum = 0,
      variance = NA_real_,
      method = "Weighted paired t-test"
    ))
  }

  weight_sum <- sum(w_num)
  weight_sq_sum <- sum(w_num^2)
  estimate <- sum(w_num * diff_num) / weight_sum
  n_eff <- (weight_sum^2) / weight_sq_sum
  denom_unbiased <- weight_sum - (weight_sq_sum / weight_sum)

  if (!is.finite(denom_unbiased) || denom_unbiased <= 0) {
    variance <- NA_real_
  } else {
    variance <- sum(w_num * (diff_num - estimate)^2) / denom_unbiased
  }

  if (!is.finite(variance) || !is.finite(n_eff) || n_eff <= 1) {
    std_error <- NA_real_
    statistic <- NA_real_
    df <- NA_real_
    p_value <- NA_real_
    conf_low <- NA_real_
    conf_high <- NA_real_
  } else {
    std_error <- sqrt(variance / n_eff)
    df <- n_eff - 1

    if (!is.finite(std_error) || std_error == 0) {
      statistic <- if (isTRUE(all.equal(estimate, 0))) 0 else sign(estimate) * Inf
      p_value <- if (isTRUE(all.equal(estimate, 0))) 1 else 0
      conf_low <- estimate
      conf_high <- estimate
    } else {
      statistic <- estimate / std_error
      p_value <- 2 * stats::pt(-abs(statistic), df = df)
      alpha <- 1 - conf_level
      crit <- stats::qt(1 - alpha / 2, df = df)
      conf_low <- estimate - crit * std_error
      conf_high <- estimate + crit * std_error
    }
  }

  list(
    estimate = estimate,
    std_error = std_error,
    statistic = statistic,
    parameter = df,
    p_value = p_value,
    conf_low = conf_low,
    conf_high = conf_high,
    n_obs = length(diff_num),
    n_eff = n_eff,
    weight_sum = weight_sum,
    variance = variance,
    method = "Weighted paired t-test"
  )
}

build_default_pairs <- function(period_ids) {
  period_ids <- unique(as.character(period_ids))
  period_ids <- period_ids[!is.na(period_ids) & nzchar(period_ids)]
  if (length(period_ids) < 2) {
    stop("Need at least two period IDs to compute changes.", call. = FALSE)
  }

  if (length(period_ids) == 2) {
    return(tibble(from_period_id = period_ids[[1]], to_period_id = period_ids[[2]]))
  }

  tibble(
    from_period_id = c(period_ids[-length(period_ids)], period_ids[[1]]),
    to_period_id = c(period_ids[-1], period_ids[[length(period_ids)]])
  )
}

resolve_period_ids <- function(cfg_yaml, observed_period_ids) {
  cfg_periods <- cfg_yaml$study_periods %||% list()
  cfg_ids <- vapply(cfg_periods, function(x) as.character(x$period_id %||% NA_character_), character(1))
  cfg_ids <- cfg_ids[!is.na(cfg_ids) & nzchar(cfg_ids)]
  observed_period_ids <- unique(as.character(observed_period_ids))
  observed_period_ids <- observed_period_ids[!is.na(observed_period_ids) & nzchar(observed_period_ids)]

  if (length(cfg_ids) > 0) {
    ordered <- cfg_ids[cfg_ids %in% observed_period_ids]
    extras <- setdiff(observed_period_ids, ordered)
    return(c(ordered, sort(extras)))
  }

  sort(observed_period_ids)
}

summarise_period_stats <- function(panel_df, weight_col, value_col) {
  weight_sym <- rlang::sym(weight_col)
  value_sym <- rlang::sym(value_col)

  panel_df %>%
    group_by(period_id) %>%
    summarise(
      total_weight = sum(!!weight_sym, na.rm = TRUE),
      citywide_avg_weighted_travel_time_penalized = weighted_mean_safe(!!value_sym, !!weight_sym),
      n_observations = n(),
      .groups = "drop"
    )
}

compare_period_pairs <- function(panel_df, period_stats, unit_keys, weight_col, value_col, pair_tbl, source_note) {
  weight_sym <- rlang::sym(weight_col)
  value_sym <- rlang::sym(value_col)

  get_period_mean <- function(pid) {
    out <- period_stats$citywide_avg_weighted_travel_time_penalized[period_stats$period_id == pid]
    if (length(out) == 0) NA_real_ else out[[1]]
  }

  pair_results <- lapply(seq_len(nrow(pair_tbl)), function(i) {
    from_pid <- pair_tbl$from_period_id[[i]]
    to_pid <- pair_tbl$to_period_id[[i]]

    from_tbl <- panel_df %>%
      filter(period_id == from_pid) %>%
      select(all_of(unit_keys), weight_from = !!weight_sym, value_from = !!value_sym)

    to_tbl <- panel_df %>%
      filter(period_id == to_pid) %>%
      select(all_of(unit_keys), weight_to = !!weight_sym, value_to = !!value_sym)

    paired <- from_tbl %>%
      inner_join(to_tbl, by = unit_keys) %>%
      mutate(
        paired_weight = dplyr::coalesce((weight_from + weight_to) / 2, weight_from, weight_to),
        delta_minutes = value_to - value_from
      ) %>%
      filter(is.finite(delta_minutes), is.finite(paired_weight), paired_weight > 0)

    test_out <- weighted_paired_t_test(paired$delta_minutes, paired$paired_weight)

    tibble(
      from_period_id = from_pid,
      to_period_id = to_pid,
      project_comparison_id = paste0(to_pid, "_vs_", from_pid),
      delta_citywide_minutes = get_period_mean(to_pid) - get_period_mean(from_pid),
      delta_paired_weighted_minutes = test_out$estimate,
      std_error = test_out$std_error,
      conf_low = test_out$conf_low,
      conf_high = test_out$conf_high,
      t_statistic = test_out$statistic,
      df = test_out$parameter,
      p_value = test_out$p_value,
      significant_p_0_05 = is.finite(test_out$p_value) & test_out$p_value < 0.05,
      n_matched_units = test_out$n_obs,
      n_eff = test_out$n_eff,
      matched_weight_sum = test_out$weight_sum,
      data_source = source_note,
      test_method = test_out$method
    )
  })

  bind_rows(pair_results)
}

build_panel_from_od_outputs <- function(run_root, analysis_unit, scenario_id, time_window_id, unreachable_penalty) {
  scenario_id_use <- scenario_id
  time_window_id_use <- time_window_id
  od_path <- file.path(run_root, "od", "od_weights_all.csv.gz")
  tt_path <- file.path(run_root, "travel_times", "period_travel_times.csv.gz")
  if (!file.exists(od_path) || !file.exists(tt_path)) {
    return(NULL)
  }

  od_all <- read_csv_guess(od_path)
  tt_all <- read_csv_guess(tt_path)

  if (nrow(od_all) == 0 || nrow(tt_all) == 0) {
    return(NULL)
  }

  od_all$weight_for_metrics <- dplyr::coalesce(
    if ("weight_sum_adjusted" %in% names(od_all)) safe_numeric(od_all$weight_sum_adjusted) else rep(NA_real_, nrow(od_all)),
    if ("weight_sum" %in% names(od_all)) safe_numeric(od_all$weight_sum) else rep(NA_real_, nrow(od_all))
  )
  tt_all$travel_time_penalized_use <- dplyr::coalesce(
    if ("travel_time_p50_penalized" %in% names(tt_all)) safe_numeric(tt_all$travel_time_p50_penalized) else rep(NA_real_, nrow(tt_all)),
    if ("travel_time_p50" %in% names(tt_all)) safe_numeric(tt_all$travel_time_p50) else rep(NA_real_, nrow(tt_all)),
    unreachable_penalty
  )

  panel_df <- od_all %>%
    transmute(
      scenario_id = as.character(.data$scenario_id),
      time_window_id = as.character(.data$time_bin),
      origin_id = standardize_zone_id(.data$origin_id, analysis_unit),
      destination_id = standardize_zone_id(.data$destination_id, analysis_unit),
      weight_for_metrics = .data$weight_for_metrics
    ) %>%
    filter(!is.na(origin_id), !is.na(destination_id), !is.na(weight_for_metrics), weight_for_metrics > 0) %>%
    inner_join(
      tt_all %>%
        transmute(
          period_id = as.character(.data$period_id),
          scenario_id = as.character(.data$od_scenario_id),
          time_window_id = as.character(.data$time_window_id),
          origin_id = standardize_zone_id(.data$from_id, analysis_unit),
          destination_id = standardize_zone_id(.data$to_id, analysis_unit),
          travel_time_penalized = .data$travel_time_penalized_use
        ) %>%
        filter(!is.na(period_id), nzchar(period_id)),
      by = c("scenario_id", "time_window_id", "origin_id", "destination_id")
    )

  if (!is.null(scenario_id_use)) {
    panel_df <- panel_df %>% filter(.data$scenario_id == scenario_id_use)
  }
  if (!is.null(time_window_id_use)) {
    panel_df <- panel_df %>% filter(.data$time_window_id == time_window_id_use)
  }

  if (nrow(panel_df) == 0) {
    return(NULL)
  }

  list(
    panel = panel_df,
    unit_keys = c("origin_id", "destination_id"),
    weight_col = "weight_for_metrics",
    value_col = "travel_time_penalized",
    source_note = "od_pair_weighted",
    selected_scenario_id = unique(panel_df$scenario_id),
    selected_time_window_id = unique(panel_df$time_window_id)
  )
}

build_panel_from_tract_metrics <- function(run_root, analysis_unit, scenario_id, time_window_id) {
  scenario_id_use <- scenario_id
  time_window_id_use <- time_window_id
  metrics_path <- file.path(run_root, "accessibility", "tract_period_metrics.csv")
  if (!file.exists(metrics_path)) {
    return(NULL)
  }

  metrics <- read_csv_guess(metrics_path)
  if (nrow(metrics) == 0) {
    return(NULL)
  }

  panel_df <- metrics %>%
    transmute(
      period_id = as.character(.data$period_id),
      scenario_id = as.character(.data$scenario_id),
      time_window_id = as.character(.data$time_window_id),
      origin_id = standardize_zone_id(.data$origin_id, analysis_unit),
      total_weight = safe_numeric(.data$total_od_weight),
      avg_weighted_travel_time_penalized = safe_numeric(.data$avg_weighted_travel_time_penalized)
    ) %>%
    filter(
      !is.na(period_id), nzchar(period_id),
      !is.na(origin_id),
      is.finite(total_weight), total_weight > 0,
      is.finite(avg_weighted_travel_time_penalized)
    )

  if (!is.null(scenario_id_use)) {
    panel_df <- panel_df %>% filter(.data$scenario_id == scenario_id_use)
  }
  if (!is.null(time_window_id_use)) {
    panel_df <- panel_df %>% filter(.data$time_window_id == time_window_id_use)
  }

  if (nrow(panel_df) == 0) {
    return(NULL)
  }

  list(
    panel = panel_df,
    unit_keys = c("origin_id"),
    weight_col = "total_weight",
    value_col = "avg_weighted_travel_time_penalized",
    source_note = "tract_weighted_fallback",
    selected_scenario_id = unique(panel_df$scenario_id),
    selected_time_window_id = unique(panel_df$time_window_id)
  )
}

resolve_selected_value <- function(values, preferred = NULL, label = "value") {
  values <- unique(as.character(values))
  values <- values[!is.na(values) & nzchar(values)]
  if (!is.null(preferred) && preferred %in% values) {
    return(preferred)
  }
  if (length(values) == 0) {
    stop(paste0("Could not resolve ", label, "."), call. = FALSE)
  }
  if (!is.null(preferred) && !(preferred %in% values)) {
    message("Requested ", label, " `", preferred, "` not found. Using `", values[[1]], "` instead.")
  } else if (length(values) > 1) {
    message("Multiple ", label, " values found. Using `", values[[1]], "`. Available: ", paste(values, collapse = ", "))
  }
  values[[1]]
}

write_summary_text <- function(path, run_root, period_stats, comparison_stats, scenario_id, time_window_id, data_source) {
  lines <- c(
    paste0("Run root: ", run_root),
    paste0("Scenario ID: ", scenario_id),
    paste0("Time window ID: ", time_window_id),
    paste0("Data source: ", data_source),
    "",
    "Period means (minutes):"
  )

  for (i in seq_len(nrow(period_stats))) {
    lines <- c(
      lines,
      paste0(
        "  ", period_stats$period_id[[i]],
        ": ", sprintf("%.3f", period_stats$citywide_avg_weighted_travel_time_penalized[[i]]),
        " (weight=", sprintf("%.3f", period_stats$total_weight[[i]]), ")"
      )
    )
  }

  lines <- c(lines, "", "Pairwise changes (to minus from):")

  for (i in seq_len(nrow(comparison_stats))) {
    lines <- c(
      lines,
      paste0(
        "  ", comparison_stats$from_period_id[[i]], " -> ", comparison_stats$to_period_id[[i]],
        ": delta=", sprintf("%.3f", comparison_stats$delta_citywide_minutes[[i]]),
        ", p=", sprintf("%.6f", comparison_stats$p_value[[i]]),
        ", significant_0.05=", ifelse(isTRUE(comparison_stats$significant_p_0_05[[i]]), "TRUE", "FALSE")
      )
    )
  }

  writeLines(lines, con = path, useBytes = TRUE)
}

script_path <- get_script_path()
project_root_default <- if (!is.null(script_path)) {
  normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
} else {
  getwd()
}

cli <- parse_cli_args(commandArgs(trailingOnly = TRUE))

cfg_yaml <- NULL
config_path <- cli$config
run_root <- cli$run_root
analysis_unit <- cli$analysis_unit
source_id <- cli$source_id
run_id <- cli$run_id %||% "latest"
scenario_id <- cli$scenario_id
time_window_id <- cli$time_window_id
output_dir <- cli$output_dir

if (is.null(config_path) && is.null(run_root)) {
  config_path <- file.path("config", "chicago.yml")
}

if (!is.null(config_path)) {
  if (!grepl("^(~|/|[A-Za-z]:)", config_path)) {
    config_path <- file.path(project_root_default, config_path)
  }
  config_path <- normalizePath(config_path, winslash = "/", mustWork = TRUE)
  cfg_yaml <- yaml::read_yaml(config_path)
}

if (!is.null(run_root)) {
  if (!grepl("^(~|/|[A-Za-z]:)", run_root)) {
    run_root <- file.path(project_root_default, run_root)
  }
  run_root <- normalizePath(run_root, winslash = "/", mustWork = TRUE)
  if (!is_run_root_dir(run_root)) {
    stop(paste0("Provided run root does not look like a run directory: ", run_root), call. = FALSE)
  }
} else {
  if (is.null(cfg_yaml)) {
    stop("Need either a config path or a run root.", call. = FALSE)
  }

  project_root <- dirname(dirname(config_path))
  city_id <- as.character(cfg_yaml$project$city_id %||% stop("Config is missing project$city_id.", call. = FALSE))
  source_id <- source_id %||% as.character(cfg_yaml$survey$source_id %||% stop("Please provide source_id, or add survey$source_id in config.", call. = FALSE))
  analysis_unit <- normalize_analysis_unit(analysis_unit %||% cfg_yaml$geography$analysis_unit %||% "tract")

  run_base_dir <- file.path(project_root, "data", "processed", city_id, "runs", source_id, analysis_unit)
  if (!dir.exists(run_base_dir)) {
    stop(paste0("Run base directory does not exist: ", run_base_dir), call. = FALSE)
  }

  if (is.null(run_id) || identical(run_id, "") || identical(run_id, "latest")) {
    run_root <- find_latest_run_root(run_base_dir)
  } else {
    run_root <- file.path(run_base_dir, run_id)
    if (!dir.exists(run_root)) {
      stop(paste0("Run folder does not exist: ", run_root), call. = FALSE)
    }
  }
}

analysis_unit <- normalize_analysis_unit(analysis_unit %||% cfg_yaml$geography$analysis_unit %||% "tract")
unreachable_penalty <- safe_numeric(cfg_yaml$routing$unreachable_penalty_minutes %||% 180)
if (!is.finite(unreachable_penalty)) unreachable_penalty <- 180

default_time_window <- cfg_yaml$map$default_time_window_id %||% NULL
if (is.null(default_time_window)) {
  rw <- cfg_yaml$routing$routing_windows %||% list()
  if (length(rw) > 0) {
    default_time_window <- as.character(rw[[1]]$time_window_id %||% NULL)
  }
}

if (is.null(scenario_id) && !is.null(cfg_yaml)) {
  rw <- cfg_yaml$routing$routing_windows %||% list()
  if (length(rw) > 0) {
    matched <- rw[vapply(rw, function(x) identical(as.character(x$time_window_id %||% NA_character_), as.character(time_window_id %||% default_time_window)), logical(1))]
    if (length(matched) > 0) {
      scenario_id <- as.character(matched[[1]]$od_scenario_id %||% NULL)
    }
  }
  if (is.null(scenario_id)) {
    od_scenarios <- cfg_yaml$od_scenarios %||% list()
    if (length(od_scenarios) > 0) {
      scenario_id <- as.character(od_scenarios[[1]]$scenario_id %||% NULL)
    }
  }
}

time_window_id <- time_window_id %||% default_time_window

od_panel_bundle <- build_panel_from_od_outputs(
  run_root = run_root,
  analysis_unit = analysis_unit,
  scenario_id = scenario_id,
  time_window_id = time_window_id,
  unreachable_penalty = unreachable_penalty
)

panel_bundle <- od_panel_bundle
if (is.null(panel_bundle)) {
  panel_bundle <- build_panel_from_tract_metrics(
    run_root = run_root,
    analysis_unit = analysis_unit,
    scenario_id = scenario_id,
    time_window_id = time_window_id
  )
}

if (is.null(panel_bundle)) {
  stop(
    paste0(
      "Could not build analysis panel from run outputs. Checked: ",
      file.path(run_root, "od", "od_weights_all.csv.gz"), ", ",
      file.path(run_root, "travel_times", "period_travel_times.csv.gz"), ", and ",
      file.path(run_root, "accessibility", "tract_period_metrics.csv")
    ),
    call. = FALSE
  )
}

scenario_id <- resolve_selected_value(panel_bundle$selected_scenario_id, scenario_id, "scenario_id")
time_window_id <- resolve_selected_value(panel_bundle$selected_time_window_id, time_window_id, "time_window_id")

panel_df <- panel_bundle$panel %>%
  filter(.data$scenario_id == scenario_id, .data$time_window_id == time_window_id)

if (nrow(panel_df) == 0) {
  stop("No rows left after filtering to the selected scenario and time window.", call. = FALSE)
}

period_ids <- resolve_period_ids(cfg_yaml, unique(panel_df$period_id))
panel_df <- panel_df %>% filter(period_id %in% period_ids)

period_stats <- summarise_period_stats(panel_df, panel_bundle$weight_col, panel_bundle$value_col) %>%
  mutate(period_id = factor(period_id, levels = period_ids)) %>%
  arrange(period_id) %>%
  mutate(period_id = as.character(period_id))

pair_tbl <- build_default_pairs(period_ids)
comparison_stats <- compare_period_pairs(
  panel_df = panel_df,
  period_stats = period_stats,
  unit_keys = panel_bundle$unit_keys,
  weight_col = panel_bundle$weight_col,
  value_col = panel_bundle$value_col,
  pair_tbl = pair_tbl,
  source_note = panel_bundle$source_note
)

output_dir <- output_dir %||% file.path(run_root, "accessibility")
fs::dir_create(output_dir)

period_out_path <- file.path(output_dir, "citywide_period_weighted_travel_time.csv")
comparison_out_path <- file.path(output_dir, "citywide_period_change_tests.csv")
summary_out_path <- file.path(output_dir, "citywide_period_change_summary.txt")

readr::write_csv(period_stats, period_out_path)
readr::write_csv(comparison_stats, comparison_out_path)
write_summary_text(
  path = summary_out_path,
  run_root = run_root,
  period_stats = period_stats,
  comparison_stats = comparison_stats,
  scenario_id = scenario_id,
  time_window_id = time_window_id,
  data_source = panel_bundle$source_note
)

message("Citywide period weighted travel times:")
print(period_stats)
message("\nCitywide period change tests:")
print(comparison_stats)
message("\nWrote:")
message("  ", period_out_path)
message("  ", comparison_out_path)
message("  ", summary_out_path)
